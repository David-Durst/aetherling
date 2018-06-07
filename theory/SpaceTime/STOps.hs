module SpaceTime.STOps where
import SpaceTime.STTypes
import SpaceTime.STMetrics
import SpaceTime.STOpClasses

-- These are leaf nodes that can be used in a higher order operator
data MappableLeafOp =
  Add TokenType
  | Sub TokenType
  | Mul TokenType
  | Div TokenType
  deriving (Eq, Show)

twoInSimplePorts t = [T_Port "I0" 1 t, T_Port "I1" 1 t]
oneOutSimplePort t = [T_Port "O" 1 t]

instance SpaceTime MappableLeafOp where
  space (Add t) = OWA (len t) (2 * len t)
  space (Sub t) = space (Add t)
  space (Mul t) = OWA (mulSpaceTimeIncreaser * len t) wireArea
    where OWA _ wireArea = space (Add t)
  space (Div t) = OWA (divSpaceTimeIncreaser * len t) wireArea
    where OWA _ wireArea = space (Add t)

  time (Add t) = SCTime 0 1
  time (Sub t) = SCTime 0 1
  time (Mul t) = SCTime 0 mulSpaceTimeIncreaser
  time (Div t) = SCTime 0 divSpaceTimeIncreaser

  util _ = 1.0

  inPortsType (Add t) = twoInSimplePorts t
  inPortsType (Sub t) = twoInSimplePorts t
  inPortsType (Mul t) = twoInSimplePorts t
  inPortsType (Div t) = twoInSimplePorts t

  outPortsType (Add t) = oneOutSimplePort t
  outPortsType (Sub t) = oneOutSimplePort t
  outPortsType (Mul t) = oneOutSimplePort t
  outPortsType (Div t) = oneOutSimplePort t
  
  numFirings _ = 1

-- These are leaf nodes that need a custom implementation to be parallelized
-- and cannot be used in a map, reduce, or iterate
data NonMappableLeafOp =
  -- Array is constant produced, int is stream length
  Constant_Int Int [Int]
  -- Array is constant produced, int is stream length
  | Constant_Bit Int [Bool]
  -- first pair is input stream length and tokens per stream element, second is output
  | StreamArrayController (Int, TokenType) (Int, TokenType)
  deriving (Eq, Show)

instance SpaceTime NonMappableLeafOp where
  space (Constant_Int n consts) = OWA (len (T_Array n T_Int)) 0
  space (Constant_Bit n consts) = OWA (len (T_Array n T_Bit)) 0
  -- just a pass through, so will get removed by CoreIR
  space (StreamArrayController (inSLen, _) (outSLen, _)) | 
    inSLen == 1 && outSLen == 1 = addId
  -- may need a more accurate approximate, but most conservative is storing
  -- entire input
  space (StreamArrayController (inSLen, inType) _) = registerSpace [inType] |* inSLen

  -- assuming reads are 
  time (Constant_Int _ _) = SCTime 0 1
  time (Constant_Bit _ _) = SCTime 0 1
  time (StreamArrayController (inSLen, _) (outSLen, _)) | 
    inSLen == 1 && outSLen == 1 = addId
  time (StreamArrayController (inSLen, _) (outSLen, _)) = registerTime |* 
    lcm inSLen outSLen

  util _ = 1.0

  inPortsType (Constant_Int _ _) = []
  inPortsType (Constant_Bit _ _) = []
  inPortsType (StreamArrayController (inSLen, inType) _) = [T_Port "I" inSLen inType]

  outPortsType (Constant_Int n ints) = [T_Port "O" n (T_Array (length ints) T_Int)]
  outPortsType (Constant_Bit n bits) = [T_Port "O" n (T_Array (length bits) T_Bit)]
  outPortsType (StreamArrayController _ (outSLen, outType)) = [T_Port "O" outSLen outType]

  numFirings _ = 1

data HigherOrderOp = 
  -- First Int is parallelism, second is total number elements reducing
  MapOp Int Int HigherOrderOp
  -- First Int is parallelism, second is total number elements reducing
  | ReduceOp Int Int HigherOrderOp
  | LeafOp MappableLeafOp
  deriving (Eq, Show)

instance SpaceTime HigherOrderOp where
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
  space (LeafOp op) = space op

  time (MapOp pEl totEl op) = replicateTimeOverStream (totEl `ceilDiv` pEl) (time op)
  time (ReduceOp pEl totEl op) | pEl == totEl = (time op) |* (ceilLog pEl)
  time (ReduceOp pEl totEl op) =
    replicateTimeOverStream (totEl `ceilDiv` pEl) (reduceTreeTime |+| (time op) |+| registerTime)
    where reduceTreeTime = time (ReduceOp pEl pEl op)
  time (LeafOp op) = time op

  util _ = 1

  inPortsType (MapOp pEl totEl op) = duplicatePorts pEl $
    scalePortsStreamLens (totEl `ceilDiv` pEl) (inPortsType op)
  inPortsType (ReduceOp pEl totEl op) = duplicatePorts pEl $
    scalePortsStreamLens (totEl `ceilDiv` pEl) (inPortsType op)
  inPortsType (LeafOp op) = inPortsType op

  outPortsType (MapOp pEl totEl op) = duplicatePorts pEl $
    scalePortsStreamLens (totEl `ceilDiv` pEl) (outPortsType op)
  outPortsType (ReduceOp _ _ op) = outPortsType op
  outPortsType (LeafOp op) = outPortsType op

  numFirings _ = 1

data SingleFiringOp =
  SFHigherOrder HigherOrderOp
  | SFMappable MappableLeafOp
  | SFNonMappable NonMappableLeafOp
  deriving (Eq, Show)

instance SpaceTime SingleFiringOp where
  space (SFHigherOrder op) = space op
  space (SFMappable op) = space op
  space (SFNonMappable op) = space op

  time (SFHigherOrder op) = time op
  time (SFMappable op) = time op
  time (SFNonMappable op) = time op

  util (SFHigherOrder op) = util op
  util (SFMappable op) = util op
  util (SFNonMappable op) = util op

  inPortsType (SFHigherOrder op) = inPortsType op
  inPortsType (SFMappable op) = inPortsType op
  inPortsType (SFNonMappable op) = inPortsType op

  outPortsType (SFHigherOrder op) = outPortsType op
  outPortsType (SFMappable op) = outPortsType op
  outPortsType (SFNonMappable op) = outPortsType op

  numFirings _ = 1


-- These are ops that require knowing the full stream length, cannot be 
-- iterated
data FixedFiringOp
  MemRead {mrStreamLen :: Int, mrT :: PortType}
  | MemWrite {mwStreamLen :: Int, mwT :: PortType}
  -- first Int is pixels per clock, second is window width, third int is 
  | LineBuffer {pxPerClock :: Int, windowWidth :: Int, lbInStreamLen :: Int,
    lbInT :: TokenType}

instance SpaceTime FixedFiringOp where
  space (Mem_Read _ t) = OWA (len t) (len t)
  space (Mem_Write _ t) = space (Mem_Read t)
  -- need counter for warmup, and registers for storing intermediate values
  -- registers account for wiring as some registers receive input wires,
  -- others get wires from other registers
  space (LineBuffer p w _ t) = counterSpace (p `ceilDiv` w) |+| 
    registerSpace [T_Array (p + w - 1) t]

  -- assuming reads are 
  time (Mem_Read sLen _) = SCTime rwTime rwTime |* sLen
  time (Mem_Write sLen _) = SCTime rwTime rwTime |* sLen
  time (LineBuffer _ _ sLen _) = registerTime |* sLen

  util _ = 1.0

  inPortsType (Mem_Read _ _) = []
  inPortsType (Mem_Write sLen t) = [T_Port "I" sLen t]
  inPortsType (LineBuffer p _ sLen t) = [T_Port "I" sLen (T_Array p t)]

  outPortsType (Mem_Read sLen t) = [T_Port "O" sLen t]
  outPortsType (Mem_Write _ _) = []
  outPortsType (LineBuffer p w sLen t) =
    [T_Port "O" (sLen - ((w `ceilDiv` p) - 1)) (T_Array p (T_Array w t))]

  numFirings _ = 1

-- Int is number of unutilized clocks
data UtilOp a = UtilOp Int a deriving (Eq, Show)

floatUsedClocks :: (SpaceTime a) => a -> Float
floatUsedClocks = fromIntegral . seqTime . time

instance (SpaceTime a) => SpaceTime (UtilOp a) where
  space (UtilOp _ op) = space op
  time (UtilOp unusedClocks op) = time op |+| SCTime unusedClocks 0
  util (UtilOp unusedClocks op) = floatUsedClocks op /
    (floatUsedClocks op + fromIntegral unusedClocks)
  inPortsType (UtilOp _ op) = inPortsType op
  outPortsType (UtilOp _ op) = outPortsType op
  numFirings _ = 1

-- Int here is numIterations, min is 1 and no max
data MultipleFiringOp = 
  IterOp Int (Compose UtilOp (SingleFiringOp))
  | FixedOp (UtilOp (FixedFiringOp))
  deriving (Eq, Show)

instance SpaceTime FixedFiringOp where
  space (Iter numIters op) = (counterSpace numIters) |+| (space op)
  space (FixedOp op) = space op

  time (Iter numIters op) = replicateTimeOverStream numIters (time op)
  time (fixedOp op) = time op

  util (Iter _ op) = util op
  util (FixedOp op) = util op

  inPortsType (Iter _ op) = inPortsType op
  inPortsType (FixedOp op) = inPortsType op

  outPortsType (Iter _ op) = outPortsType op
  outPortsType (FixedOp op) = outPortsType op

  numFirings (Iter n op) = n * (numFirings op)
  numFirings (FixedOp op) = numFirings op
