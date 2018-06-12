module SpaceTime.STOps where
import SpaceTime.STTypes
import SpaceTime.STMetrics
import SpaceTime.STOpClasses

-- These are leaf nodes that can be used in a higher order operator
data LeafOp =
  Add TokenType
  | Sub TokenType
  | Mul TokenType
  | Div TokenType
  | MemRead {mrStreamLen :: Int, mrT :: TokenType}
  | MemWrite {mwStreamLen :: Int, mwT :: TokenType}
  -- first Int is pixels per clock, second is window width, third int is 
  | LineBuffer {pxPerClock :: Int, windowWidth :: Int, lbInT :: TokenType}
  -- Array is constant produced, int is stream length
  | Constant_Int Int [Int]
  -- Array is constant produced, int is stream length
  | Constant_Bit Int [Bool]
  -- first pair is input stream length and tokens per stream element, second is output
  | StreamArrayController (Int, TokenType) (Int, TokenType)
  deriving (Eq, Show)

twoInSimplePorts t = [T_Port "I0" 1 t, T_Port "I1" 1 t]
oneOutSimplePort t = [T_Port "O" 1 t]

instance SpaceTime LeafOp where
  space (Add t) = OWA (len t) (2 * len t)
  space (Sub t) = space (Add t)
  space (Mul t) = OWA (mulSpaceTimeIncreaser * len t) wireArea
    where OWA _ wireArea = space (Add t)
  space (Div t) = OWA (divSpaceTimeIncreaser * len t) wireArea
    where OWA _ wireArea = space (Add t)
  space (MemRead _ t) = OWA (len t) (len t)
  space (MemWrite n t) = space (MemRead n t)
  -- need registers for storing intermediate values
  -- registers account for wiring as some registers receive input wires,
  -- others get wires from other registers
  -- add |+| counterSpace (p `ceilDiv` w) when accounting for warmup counter
  space (LineBuffer p w t) = registerSpace [T_Array (p + w - 1) t]
  space (Constant_Int n consts) = OWA (len (T_Array n T_Int)) 0
  space (Constant_Bit n consts) = OWA (len (T_Array n T_Bit)) 0
  -- just a pass through, so will get removed by CoreIR
  space (StreamArrayController (inSLen, _) (outSLen, _)) | 
    inSLen == 1 && outSLen == 1 = addId
  -- may need a more accurate approximate, but most conservative is storing
  -- entire input
  space (StreamArrayController (inSLen, inType) _) = registerSpace [inType] |* inSLen

  time (Add t) = SCTime 0 1
  time (Sub t) = SCTime 0 1
  time (Mul t) = SCTime 0 mulSpaceTimeIncreaser
  time (Div t) = SCTime 0 divSpaceTimeIncreaser
  time (MemRead sLen _) = SCTime rwTime rwTime |* sLen
  time (MemWrite sLen _) = SCTime rwTime rwTime |* sLen
  time (LineBuffer _ _ _) = registerTime
  time (Constant_Int _ _) = SCTime 0 1
  time (Constant_Bit _ _) = SCTime 0 1
  time (StreamArrayController (inSLen, _) (outSLen, _)) | 
    inSLen == 1 && outSLen == 1 = addId
  time (StreamArrayController (inSLen, _) (outSLen, _)) = registerTime |* 
    lcm inSLen outSLen

  -- what's correct here for streamarraycontroller for non-zero seqtime?
  -- for everythign else, emitting something every clock if sequential, and 
  -- no impact on clocks if combinational
  pipelineTime op | (seqTime $ time op) > 0 = PTime 1 1
  pipelineTime op = PTime 1 0

  util _ = 1.0

  inPortsType (Add t) = twoInSimplePorts t
  inPortsType (Sub t) = twoInSimplePorts t
  inPortsType (Mul t) = twoInSimplePorts t
  inPortsType (Div t) = twoInSimplePorts t
  inPortsType (MemRead _ _) = []
  inPortsType (MemWrite sLen t) = [T_Port "I" sLen t]
  inPortsType (LineBuffer p _ t) = [T_Port "I" 1 (T_Array p t)]
  inPortsType (Constant_Int _ _) = []
  inPortsType (Constant_Bit _ _) = []
  inPortsType (StreamArrayController (inSLen, inType) _) = [T_Port "I" inSLen inType]

  outPortsType (Add t) = oneOutSimplePort t
  outPortsType (Sub t) = oneOutSimplePort t
  outPortsType (Mul t) = oneOutSimplePort t
  outPortsType (Div t) = oneOutSimplePort t
  outPortsType (MemRead sLen t) = [T_Port "O" sLen t]
  outPortsType (MemWrite _ _) = []
  -- go back to (sLen - ((w `ceilDiv` p) - 1)) for out stream length when 
  -- including warmup and shutdown
  outPortsType (LineBuffer p w t) = [T_Port "O" 1 (T_Array p (T_Array w t))]
  outPortsType (Constant_Int n ints) = [T_Port "O" n (T_Array (length ints) T_Int)]
  outPortsType (Constant_Bit n bits) = [T_Port "O" n (T_Array (length bits) T_Bit)]
  outPortsType (StreamArrayController _ (outSLen, outType)) = [T_Port "O" outSLen outType]
  
  numFirings _ = 1
  

data SingleFiringOp = 
  -- First Int is parallelism, second is total number elements reducing
  MapOp Int Int SingleFiringOp
  -- First Int is parallelism, second is total number elements reducing
  -- can't reduce a mem_read
  | ReduceOp Int Int SingleFiringOp
  | SFLeafOp LeafOp
  deriving (Eq, Show)

instance SpaceTime SingleFiringOp where
  -- area of parallel map is area of all the copies
  space (MapOp pEl _ op) = (space op) |* pEl
  -- area of reduce is area of reduce tree, with area for register for partial
  -- results and counter for tracking iteration time if input is stream of more
  -- than what is passed in one clock
  space (ReduceOp pEl totEl op) | pEl == totEl = (space op) |* (pEl - 1)
  space rOp@(ReduceOp pEl _ op) =
    reduceTreeSpace |+| (space op) |+| (registerSpace $ map pTType $ outPortsType op)
    |+| (counterSpace $ seqTime $ time rOp)
    where reduceTreeSpace = space (ReduceOp pEl pEl op)
  space (SFLeafOp op) = space op

  time (MapOp pEl totEl op) = replicateTimeOverStream (totEl `ceilDiv` pEl) (time op)
  time (ReduceOp pEl totEl op) | pEl == totEl = (time op) |* (ceilLog pEl)
  time (ReduceOp pEl totEl op) =
    replicateTimeOverStream (totEl `ceilDiv` pEl) (reduceTreeTime |+| (time op) |+| registerTime)
    where reduceTreeTime = time (ReduceOp pEl pEl op)
  time (SFLeafOp op) = time op

  pipelineTime (MapOp _ _ op) = pipelineTime op
  pipelineTime (ReduceOp pEl totEl op) | pEl == totEl = (pipelineTime op) |* (ceilLog pEl)
  pipelineTime (ReduceOp pEl totEl op) = reduceTreeTime |* (totEl `ceilDiv` pEl)
    where reduceTreeTime = pipelineTime (ReduceOp pEl pEl op)
  pipelineTime (SFLeafOp op) = pipelineTime op

  util _ = 1

  inPortsType (MapOp pEl totEl op) = duplicatePorts pEl $
    scalePortsStreamLens (totEl `ceilDiv` pEl) (inPortsType op)
  inPortsType (ReduceOp pEl totEl op) = duplicatePorts pEl $
    scalePortsStreamLens (totEl `ceilDiv` pEl) (inPortsType op)
  inPortsType (SFLeafOp op) = inPortsType op

  outPortsType (MapOp pEl totEl op) = duplicatePorts pEl $
    scalePortsStreamLens (totEl `ceilDiv` pEl) (outPortsType op)
  outPortsType (ReduceOp _ _ op) = outPortsType op
  outPortsType (SFLeafOp op) = outPortsType op

  numFirings _ = 1

data (SpaceTime a) => IterOp a = 
  -- First Int is num iterations, second is num iterations active
  IterOp Int Int a
  -- Int is number of clocks doing nothing, first is inports, second is outports
  | IterTomb Int [PortType] [PortType]
  deriving (Eq, Show)

floatUsedClocks :: (SpaceTime a) => a -> Float
floatUsedClocks = fromIntegral . seqTime . time

instance (SpaceTime a) => SpaceTime (UtilOp a) where
  space iOp@(IterOp numIters _ op) = space op |+| counterSpace (seqTime $ time iOp)
  space (IterTomb numClocks iPorts _) = counterSpace numClocks |+| (registerSpace $ map pTType iPorts)

  time (IterOp numIters _ op) = replicateTimeOverStream numIters (time op)
  time (IterTomb numClocks _ _) = numClocks

  pipelineTime (IterOp numIters _ op) = pipelineTime op |* numIters
  pipelineTime (IterTomb numClocks _ _) = PTime 1 numClocks

  util (UtilOp totalIters usedIters op) = floatUsedClocks op * (fromIntegral usedIters) /
    (fromIntegral $ totalIters + usedIters)
  util (UtilTomb _ _ _) = 0

  inPortsType (UtilOp _ usedIters op) = scalePortsStreamLens usedIters $ inPortsType op
  inPortsType (UtilTomb _ iPorts _) = iPorts

  outPortsType (UtilOp _ usedIters op) = scalePortsStreamLens usedIters $ outPortsType op
  inPortsType (UtilTomb _ oPorts _) = oPorts

  numFirings (UtilOp _ usedIters op) = usedIters * numFirings op
  numFirings (UtilTomb _ _ _) = 1

fullUtilSFToIter :: Int -> SingleFiringOp -> Compose (IterOp (Compose (IterOp SingleFiringOp)))
fullUtilSFToIter n sfOp = ComposeContainer $ IterOp n $ ComposeContainer $ UtilOp 0 sfOp
