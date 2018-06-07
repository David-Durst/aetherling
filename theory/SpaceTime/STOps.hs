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

twoInSimplePorts t = portsFromTokens [("I0", 1, 1, t), ("I1", 1, 1, t)]
oneOutSimplePort t = portsFromTokens [("O", 1, 1, t)]

instance SpaceTime LeafOp where
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
  Mem_Read TokenType
  | Mem_Write TokenType deriving (Eq, Show)
  -- Array is constant produced, int is stream length
  | Constant_Int Int [Int]
  -- Array is constant produced, int is stream length
  | Constant_Bit Int [Bool]
  -- first Int is pixels per clock, second is window width
  | LineBuffer Int Int TokenType
  -- first pair is input stream length and tokens per stream element, second is output
  | StreamArrayController (Int, TokensType) (Int, TokensType)

instance SpaceTime NonMappableLeafOp where
  space (Mem_Read t) = OWA (len t) (len t)
  space (Mem_Write t) = space (Mem_Read t)
  space (Constant_Int n consts) = OWA (len (T_Array n T_Int)) 0
  space (Constant_Bit n consts) = OWA (len (T_Array n T_Bit)) 0
  -- need counter for warmup, and registers for storing intermediate values
  -- registers account for wiring as some registers receive input wires,
  -- others get wires from other registers
  space (LineBuffer p w t) = counterSpace (p / w) |+| 
    registerSpace [T_Array (p + w - 1) t]
  -- may need a more accurate approximate, but most conservative is storing
  -- entire input
  space StreamArrayController (inSLen, inType) _ = registerSpace [inType] |* inSLen

  -- assuming reads are 
  time (Mem_Read _) = SCTime rwTime rwTime
  time (Mem_Write _) = SCTime rwTime rwTime
  time (Constant_Int _ _) = SCTime 0 1
  time (Constant_Bit _ _) = SCTime 0 1
  time (LineBuffer _ _ _) = registerTime
  time (StreamArrayController (inSLen _) (outSLen _)) = registerTime |* 
    lcm inSLen outSLen

  util _ = 1.0

  inPortsType (Mem_Read _) = []
  inPortsType (Mem_Write t) = oneOutSimplePort t
  inPortsType (Constant_Int _ _) = []
  inPortsType (Constant_Bit _ _) = []
  inPortsType (LineBuffer p _ t) = [T_Port "I" 1 (T_Array p t)]
  inPortsType (StreamArrayController (inSLen inType) _) = [T_Port "I" inSLen inType]

  outPortsType (Mem_Read t) = portsFromTokens [("I", 1, 1, t)]
  outPortsType (Mem_Write _) = []
  outPortsType (Constant_Int n ints) = [T_Port "O" n (length ints) T_Int]
  outPortsType (Constant_Bit n bits) = [T_Port "O" n (length bits) T_Bit]
  outPortsType (LineBuffer p w t) = [T_Port "O" 1 (T_Array p t)]
  outPortsType (StreamArrayController _ (outSLen outType)) = [T_Port "O" outSLen outType]

  numFirings _ = 1

-- ParParams handles parallelism and inverse parallelism (aka underutilization)
-- for a single firing
-- utilizedClocks / allClocksInStream = pct of a firing that this op is utilized
data ParParams = ParParams { parallelism :: Int, utilizedClocks :: Int,
  allClocksInStream :: Int } deriving (Eq, Show)

-- leaf nodes define what is done for one token in one firing, and 
-- can scale them up or down across one firing and multiple firings
--
-- These are the scheduling ops for building types of pipelines in the lower, 
-- scheduled IR. 
-- Can schedule in two dimenions - over multiple firings (for handling more tokens)
-- and a single firing (for doing wider or narrower tokens over different
-- numbers of clocks)
-- iter handles scaling all operations over multiple firings
-- map, reduce, and fold handle scaling operations in single firing
-- Must handle 0 or more tokens so the min of stream dimension is 0
-- Must handle 0 or more tokens per clock so the min of token dimension is 0

-- add a compose here, maybe later
data SingleFiringOp = 
  MapSF ParParams SingleFiringOp
  | ReduceSF ParParams SingleFiringOp
  | ArithmeticSF ArithmeticOp
  | MemorySF MemoryOp
  | ComposeParSF [SingleFiringOp]
  | ComposeSeqSF [SingleFiringOp]
  deriving (Eq, Show)


instance SpaceTime SingleFiringOp where
  -- area of parallel map is area of all the copies
  space (MapSF ParParams{parallelism = p} op) = (space op) |* p
  -- area of reduce is area of reduce tree, with area for register for partial
  -- results if a signle firing is more than 1 clock
  space (ReduceSF (ParParams p uc _) op) | uc == 1 = (space op) |* (p-1)
  space (ReduceSF (ParParams p uc _) op) =
    reduceTreeSpace |+| (space op) |+| (registerSpace $ map pTType $ outPortsType op)
    where reduceTreeSpace = space (ReduceSF (ParParams p 1 1) op)
  space (ArithmeticSF op) = space op
  space (MemorySF op) = space op
  space (ComposeParSF ops) = spaceCompose ops
  space (ComposeSeqSF ops) = spaceCompose ops

  time (MapSF ParParams{allClocksInStream = ac} op) =
    replicateTimeOverStream ac (time op)
  time (ReduceSF (ParParams p _ ac) op) | ac == 1 = (time op) |* (ceilLog p)
  time (ReduceSF (ParParams p _ ac) op) =
    replicateTimeOverStream ac (reduceTreeTime |+| (time op) |+| registerTime)
    where reduceTreeTime = time (ReduceSF (ParParams p 1 1) op)
  time (ArithmeticSF op) = time op
  time (MemorySF op) = time op
  time (ComposeParSF ops) = timeComposePar ops
  time (ComposeSeqSF ops) = timeComposeSeq ops

  util (MapSF (ParParams _ uc ac) op) = (util op) * (fromIntegral uc) / (fromIntegral ac)
  util (ReduceSF (ParParams _ uc ac) op) = (util op) * (fromIntegral uc) / (fromIntegral ac)
  util (ArithmeticSF op) = util op
  util (MemorySF op) = util op
  util (ComposeParSF ops) = utilWeightedByArea ops
  util (ComposeSeqSF ops) = utilWeightedByArea ops

  inPortsType (MapSF (ParParams p uc _) op) = duplicatePorts p $
    scalePortsStreamLens uc (inPortsType op)
  inPortsType (ReduceSF (ParParams p uc _) op) = duplicatePorts p $
    scalePortsStreamLens uc (inPortsType op)
  inPortsType (ArithmeticSF op) = inPortsType op
  inPortsType (MemorySF op) = inPortsType op
  inPortsType (ComposeParSF ops) = inPortsTypeComposePar ops
  inPortsType (ComposeSeqSF ops) = inPortsTypeComposeSeq ops

  outPortsType (MapSF (ParParams p uc _) op) = duplicatePorts p $
    scalePortsStreamLens uc (outPortsType op)
  outPortsType (ReduceSF _ op) = outPortsType op
  outPortsType (ArithmeticSF op) = outPortsType op
  outPortsType (MemorySF op) = outPortsType op
  outPortsType (ComposeParSF ops) = outPortsTypeComposePar ops
  outPortsType (ComposeSeqSF ops) = outPortsTypeComposeSeq ops

  numFirings _ = 1

instance Composable SingleFiringOp where 
  (|.|) (Just op0@(ComposeSeqSF ops0)) (Just op1@(ComposeSeqSF ops1)) | canComposeSeq op0 op1 =
    Just $ ComposeSeqSF $ ops1 ++ ops0
  (|.|) (Just op0@(ComposeSeqSF ops0)) (Just op1) | canComposeSeq op0 op1 =
    Just $ ComposeSeqSF $ [op1] ++ ops0
  (|.|) (Just op0) (Just op1@(ComposeSeqSF ops1)) | canComposeSeq op0 op1 =
    Just $ ComposeSeqSF $ ops1 ++ [op0]
  (|.|) (Just op0) (Just op1) | canComposeSeq op0 op1 = Just $ ComposeSeqSF [op1, op0]
  (|.|) _ _ = Nothing

  (|&|) (Just op0@(ComposeParSF ops0)) (Just op1@(ComposeParSF ops1)) | canComposePar op0 op1 =
    Just $ ComposeParSF $ ops0 ++ ops1
  (|&|) (Just op0@(ComposeParSF ops0)) (Just op1) | canComposePar op0 op1 =
    Just $ ComposeParSF $ ops0 ++ [op1]
  (|&|) (Just op0) (Just op1@(ComposeParSF ops1)) | canComposePar op0 op1 =
    Just $ ComposeParSF $ [op0] ++ ops1
  (|&|) (Just op0) (Just op1) | canComposePar op0 op1 = Just $ ComposeParSF [op0, op1]
  (|&|) _ _ = Nothing

data MultipleFiringOp = 
  -- Int here is numIterations, min is 1 and no max
  Iter Int SingleFiringOp
  | ComposeParMF [MultipleFiringOp]
  | ComposeSeqMF [MultipleFiringOp]
  deriving (Eq, Show)

instance SpaceTime MultipleFiringOp where
  -- when mapping over sequence, area is time to count over sequence plus 
  -- area of stuff that is being applied to each element of sequence
  space (Iter numIters op) = (counterSpace numIters) |+| (space op)
  space (ComposeParMF ops) = spaceCompose ops
  space (ComposeSeqMF ops) = spaceCompose ops

  time (Iter numIters op) = replicateTimeOverStream numIters (time op)
  time (ComposeParMF ops) = timeComposePar ops
  time (ComposeSeqMF ops) = timeComposeSeq ops

  util (Iter _ op) = util op
  util (ComposeParMF ops) = utilWeightedByArea ops
  util (ComposeSeqMF ops) = utilWeightedByArea ops

  inPortsType (Iter _ op) = inPortsType op
  inPortsType (ComposeParMF ops) = inPortsTypeComposePar ops
  inPortsType (ComposeSeqMF ops) = inPortsTypeComposeSeq ops

  outPortsType (Iter _ op) = outPortsType op
  outPortsType (ComposeParMF ops) = outPortsTypeComposePar ops
  outPortsType (ComposeSeqMF ops) = outPortsTypeComposeSeq ops

  numFirings (Iter n op) = n * (numFirings op)
  numFirings (ComposeParMF _) = 1
  numFirings (ComposeSeqMF _) = 1

-- Is there a way to get rid of this duplicate code?
instance Composable MultipleFiringOp where 
  (|.|) (Just op0@(ComposeSeqMF ops0)) (Just op1@(ComposeSeqMF ops1)) | canComposeSeq op0 op1 =
    Just $ ComposeSeqMF $ ops1 ++ ops0
  (|.|) (Just op0@(ComposeSeqMF ops0)) (Just op1) | canComposeSeq op0 op1 =
    Just $ ComposeSeqMF $ [op1] ++ ops0
  (|.|) (Just op0) (Just op1@(ComposeSeqMF ops1)) | canComposeSeq op0 op1 =
    Just $ ComposeSeqMF $ ops1 ++ [op0]
  (|.|) (Just op0) (Just op1) | canComposeSeq op0 op1 = Just $ ComposeSeqMF [op1, op0]
  (|.|) _ _ = Nothing

  (|&|) (Just op0@(ComposeParMF ops0)) (Just op1@(ComposeParMF ops1)) | canComposePar op0 op1 =
    Just $ ComposeParMF $ ops0 ++ ops1
  (|&|) (Just op0@(ComposeParMF ops0)) (Just op1) | canComposePar op0 op1 =
    Just $ ComposeParMF $ ops0 ++ [op1]
  (|&|) (Just op0) (Just op1@(ComposeParMF ops1)) | canComposePar op0 op1 =
    Just $ ComposeParMF $ [op0] ++ ops1
  (|&|) (Just op0) (Just op1) | canComposePar op0 op1 = Just $ ComposeParMF [op0, op1]
  (|&|) _ _ = Nothing
