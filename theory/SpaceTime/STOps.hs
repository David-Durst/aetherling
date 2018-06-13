{-# LANGUAGE StandaloneDeriving, ExistentialQuantification #-}
module SpaceTime.STOps where
import SpaceTime.STTypes
import SpaceTime.STMetrics
import SpaceTime.STOpClasses
import Data.Typeable

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
  pipelineTime op = PTime 1 1

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
  forall a. (SpaceTime a, Eq a, Show a, Typeable a) => MapOp Int Int a
  -- Should I use a GADT to verify op for reduce has 2 inputs only?
  -- First Int is parallelism, second is total number elements reducing
  -- can't reduce a mem_read
  | forall a. (SpaceTime a, Eq a, Show a, Typeable a) => ReduceOp Int Int a

deriving instance Show SingleFiringOp

instance Eq SingleFiringOp where
  (==) (MapOp pEl0 totEl0 op0) (MapOp pEl1 totEl1 op1) = case cast op1 of 
    Nothing -> False 
    Just op1T0 -> op0 == op1T0 && pEl0 == pEl1 && totEl0 == totEl1
  (==) (ReduceOp pEl0 totEl0 op0) (ReduceOp pEl1 totEl1 op1) = case cast op1 of 
    Nothing -> False 
    Just op1T0 -> op0 == op1T0 && pEl0 == pEl1 && totEl0 == totEl1
  (/=) op0 op1 = not (op0 /= op1)

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

  time (MapOp pEl totEl op) = replicateTimeOverStream (totEl `ceilDiv` pEl) (time op)
  time (ReduceOp pEl totEl op) | pEl == totEl = (time op) |* (ceilLog pEl)
  time (ReduceOp pEl totEl op) =
    replicateTimeOverStream (totEl `ceilDiv` pEl) (reduceTreeTime |+| (time op) |+| registerTime)
    where reduceTreeTime = time (ReduceOp pEl pEl op)

  pipelineTime (MapOp _ _ op) = pipelineTime op
  pipelineTime (ReduceOp pEl totEl op) | pEl == totEl = (pipelineTime op) |* (ceilLog pEl)
  pipelineTime (ReduceOp pEl totEl op) = reduceTreeTime |* (totEl `ceilDiv` pEl)
    where reduceTreeTime = pipelineTime (ReduceOp pEl pEl op)

  util (MapOp _ _ op) = util op
  util (ReduceOp _ _ op) = util op

  inPortsType (MapOp pEl totEl op) = duplicatePorts pEl $
    scalePortsStreamLens (totEl `ceilDiv` pEl) (inPortsType op)
  inPortsType (ReduceOp pEl totEl op) = duplicatePorts pEl $
  -- can just take head as can only reduce binary operators
  -- parallelism means apply binary to pEl at a time
    scalePortsStreamLens (totEl `ceilDiv` pEl) [head $ inPortsType op]

  outPortsType (MapOp pEl totEl op) = duplicatePorts pEl $
    scalePortsStreamLens (totEl `ceilDiv` pEl) (outPortsType op)
  outPortsType (ReduceOp _ _ op) = outPortsType op

  numFirings (MapOp _ _ op) = numFirings op
  numFirings (ReduceOp _ _ op) = numFirings op

data IterOp a = 
  -- First Int is num iterations, second is num iterations active
  IterOp Int Int a
  -- delayStages is stages to add, delayClocks is clocks to add to each stage 
  -- when doing nothing, a is for thing to wrap with registers
  -- put registers after each stage of spacetime being wrapped
  | RegDelay {delayStages :: Int, delayClocks :: Int, wrappedModule :: a}
  deriving (Eq, Show)

floatUsedClocks :: (SpaceTime a) => a -> Float
floatUsedClocks = fromIntegral . seqTime . time

instance (SpaceTime a) => SpaceTime (IterOp a) where
  space iOp@(IterOp numIters _ op) = space op |+| counterSpace (seqTime $ time iOp)
  space (RegDelay ds dc op) = counterSpace dc |+| 
    ((registerSpace $ map pTType $ inPortsType op) |* (ds + (numStages $ pipelineTime op))) |+| space op

  time (IterOp numIters _ op) = replicateTimeOverStream numIters (time op)
  time (RegDelay ds dc op) = time op |+| 
    SCTime (dc * (ds + (numStages $ pipelineTime op))) 0

  pipelineTime (IterOp numIters _ op) = pipelineTime op
  pipelineTime (RegDelay ds dc op) = PTime (ds + (numStages $ pipelineTime op)) 
    (dc + (numClocks $ pipelineTime op))

  util (IterOp totalIters usedIters op) = floatUsedClocks op * (fromIntegral usedIters) /
    (fromIntegral $ totalIters + usedIters)
  -- ignoring dleay stages for now, need to come back to them later
  util (RegDelay _ dc op) = (util op) / (util op + (fromIntegral dc))

  inPortsType (IterOp _ usedIters op) = scalePortsStreamLens usedIters $ inPortsType op
  inPortsType (RegDelay _ _ op) = inPortsType op

  outPortsType (IterOp _ usedIters op) = scalePortsStreamLens usedIters $ outPortsType op
  outPortsType (RegDelay _ _ op) = outPortsType op

  numFirings (IterOp _ usedIters op) = usedIters * numFirings op
  numFirings (RegDelay _ _ op) = numFirings op

fullIterSF :: (SpaceTime a) => Int -> Int -> a -> Compose
fullIterSF t u stOp = ComposeContainer $ IterOp t u stOp
