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

twoInSimplePorts t = [T_Port "I0" 1 (T_Array 1 t), T_Port "I1" 1 (T_Array 1 t)]
oneOutSimplePort t = [T_Port "O" 1 (T_Array 1 t)]

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
  Mem_Read TokensType
  | Mem_Write TokensType
  -- Array is constant produced, int is stream length
  | Constant_Int Int [Int]
  -- Array is constant produced, int is stream length
  | Constant_Bit Int [Bool]
  -- first Int is pixels per clock, second is window width
  | LineBuffer Int Int TokenType
  -- flattens an array of tokens
  | Flatten TokensType
  -- Split an array of tokens into arrays of length p over a stream
  | Partition Int TokensType
  -- first pair is input stream length and tokens per stream element, second is output
  | StreamArrayController (Int, TokensType) (Int, TokensType)
  deriving (Eq, Show)

instance SpaceTime NonMappableLeafOp where
  space (Mem_Read t) = OWA (len t) (len t)
  space (Mem_Write t) = space (Mem_Read t)
  space (Constant_Int n consts) = OWA (len (T_Array n T_Int)) 0
  space (Constant_Bit n consts) = OWA (len (T_Array n T_Bit)) 0
  -- need counter for warmup, and registers for storing intermediate values
  -- registers account for wiring as some registers receive input wires,
  -- others get wires from other registers
  space (LineBuffer p w t) = counterSpace (p `ceilDiv` w) |+| 
    registerSpace [T_Array (p + w - 1) t]
  space (Flatten _) = OWA 0 0
  -- need to come back and make this more accurate later
  -- as the first part of partition is pass through, no need for registers
  space (Partition _ t) = registerSpace [t]
  -- may need a more accurate approximate, but most conservative is storing
  -- entire input
  space (StreamArrayController (inSLen, inType) _) = registerSpace [inType] |* inSLen

  -- assuming reads are 
  time (Mem_Read _) = SCTime rwTime rwTime
  time (Mem_Write _) = SCTime rwTime rwTime
  time (Constant_Int _ _) = SCTime 0 1
  time (Constant_Bit _ _) = SCTime 0 1
  time (LineBuffer _ _ _) = registerTime
  time (Flatten _) = SCTime 0 1
  time (Partition p (T_Array arrLen _)) = registerTime |* (p / arrLen)
  time (StreamArrayController (inSLen, _) (outSLen, _)) = registerTime |* 
    lcm inSLen outSLen

  util _ = 1.0

  inPortsType (Mem_Read _) = []
  inPortsType (Mem_Write t) = [T_Port "I" 1 t]
  inPortsType (Constant_Int _ _) = []
  inPortsType (Constant_Bit _ _) = []
  inPortsType (LineBuffer p _ t) = [T_Port "I" 1 (T_Array p t)]
  inPortsType (StreamArrayController (inSLen, inType) _) = [T_Port "I" inSLen inType]

  outPortsType (Mem_Read t) = [T_Port "O" 1 t]
  outPortsType (Mem_Write _) = []
  outPortsType (Constant_Int n ints) = [T_Port "O" n (T_Array (length ints) T_Int)]
  outPortsType (Constant_Bit n bits) = [T_Port "O" n (T_Array (length bits) T_Bit)]
  outPortsType (LineBuffer p w t) = [T_Port "O" 1 (T_Array p t)]
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

-- Int is number of unutilized clocks
data UtilizationOp =
  UtilMapLeaf Int MappableLeafOp
  | UtilNonMapLeaf Int NonMappableLeafOp
  | UtilHigherOrder Int HigherOrderOp
  deriving (Eq, Show)

floatUsedClocks :: (SpaceTime a) => a -> Float
floatUsedClocks = fromIntegral . seqTime . time

instance SpaceTime UtilizationOp where
  space (UtilMapLeaf _ op) = space op
  space (UtilNonMapLeaf _ op) = space op
  space (UtilHigherOrder _ op) = space op

  time (UtilMapLeaf unusedClocks op) = time op |+| SCTime unusedClocks 0
  time (UtilNonMapLeaf unusedClocks op) = time op |+| SCTime unusedClocks 0
  time (UtilHigherOrder unusedClocks op) = time op |+| SCTime unusedClocks 0

  util (UtilMapLeaf unusedClocks op) = floatUsedClocks op / 
    (floatUsedClocks op + fromIntegral unusedClocks)
  util (UtilNonMapLeaf unusedClocks op) = floatUsedClocks op / 
    (floatUsedClocks op + fromIntegral unusedClocks)
  util (UtilHigherOrder unusedClocks op) = floatUsedClocks op / 
    (floatUsedClocks op + fromIntegral unusedClocks)

  inPortsType (UtilMapLeaf _ op) = inPortsType op
  inPortsType (UtilNonMapLeaf _ op) = inPortsType op
  inPortsType (UtilHigherOrder _ op) = inPortsType op

  outPortsType (UtilMapLeaf _ op) = outPortsType op
  outPortsType (UtilNonMapLeaf _ op) = outPortsType op
  outPortsType (UtilHigherOrder _ op) = outPortsType op

  numFirings _ = 1

data SingleFiringOpComposition =
  ComposeParSF [HigherOrderOp]
  | ComposeSeqSF [HigherOrderOp]
  deriving (Eq, Show)

instance SpaceTime SingleFiringOpComposition where
  space (ComposeParSF ops) = spaceCompose ops
  space (ComposeSeqSF ops) = spaceCompose ops

  time (ComposeParSF ops) = timeComposePar ops
  time (ComposeSeqSF ops) = timeComposeSeq ops

  util (ComposeParSF ops) = utilWeightedByArea ops
  util (ComposeSeqSF ops) = utilWeightedByArea ops

  inPortsType (ComposeParSF ops) = inPortsTypeComposePar ops
  inPortsType (ComposeSeqSF ops) = inPortsTypeComposeSeq ops

  outPortsType (ComposeParSF ops) = outPortsTypeComposePar ops
  outPortsType (ComposeSeqSF ops) = outPortsTypeComposeSeq ops

  numFirings _ = 1

instance Composable SingleFiringOpComposition where 
  (|.|) (Just op0@(ComposeSeqSF ops0)) (Just op1@(ComposeSeqSF ops1)) | canComposeSeq op0 op1 =
    Just $ ComposeSeqSF $ ops1 ++ ops0
  (|.|) _ _ = Nothing

  (|&|) (Just op0@(ComposeParSF ops0)) (Just op1@(ComposeParSF ops1)) | canComposePar op0 op1 =
    Just $ ComposeParSF $ ops0 ++ ops1
  (|&|) _ _ = Nothing

-- Int here is numIterations, min is 1 and no max
data MultipleFiringOps = Iter Int SingleFiringOpComposition
  deriving (Eq, Show)

instance SpaceTime MultipleFiringOps where
  space (Iter numIters op) = (counterSpace numIters) |+| (space op)
  time (Iter numIters op) = replicateTimeOverStream numIters (time op)
  util (Iter _ op) = util op
  inPortsType (Iter _ op) = inPortsType op
  outPortsType (Iter _ op) = outPortsType op
  numFirings (Iter n op) = n * (numFirings op)

data MultipleFiringOpsComposition =
  ComposeParMF [MultipleFiringOps]
  | ComposeSeqMF [MultipleFiringOps]
  deriving (Eq, Show)

instance SpaceTime MultipleFiringOpsComposition where
  space (ComposeParMF ops) = spaceCompose ops
  space (ComposeSeqMF ops) = spaceCompose ops

  time (ComposeParMF ops) = timeComposePar ops
  time (ComposeSeqMF ops) = timeComposeSeq ops

  util (ComposeParMF ops) = utilWeightedByArea ops
  util (ComposeSeqMF ops) = utilWeightedByArea ops

  inPortsType (ComposeParMF ops) = inPortsTypeComposePar ops
  inPortsType (ComposeSeqMF ops) = inPortsTypeComposeSeq ops

  outPortsType (ComposeParMF ops) = outPortsTypeComposePar ops
  outPortsType (ComposeSeqMF ops) = outPortsTypeComposeSeq ops

  numFirings (ComposeParMF _) = 1
  numFirings (ComposeSeqMF _) = 1

-- Is there a way to get rid of this duplicate code?
instance Composable MultipleFiringOpsComposition where 
  (|.|) (Just op0@(ComposeSeqMF ops0)) (Just op1@(ComposeSeqMF ops1)) | canComposeSeq op0 op1 =
    Just $ ComposeSeqMF $ ops1 ++ ops0
  (|.|) _ _ = Nothing

  (|&|) (Just op0@(ComposeParMF ops0)) (Just op1@(ComposeParMF ops1)) | canComposePar op0 op1 =
    Just $ ComposeParMF $ ops0 ++ ops1
  (|&|) _ _ = Nothing
