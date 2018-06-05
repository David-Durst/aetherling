module SpaceTime.STCompute where
import SpaceTime.STHelpers
-- the typeclasses that all the elements of the IR must implement

class SpaceTime a where
  space :: a -> OpsWireArea
  -- this is the time to process one or more firings
  time :: a -> SeqCombTime
  -- local vs global utilization? - I think this handles both well enough 
  -- for now, can revisit later
  util :: a -> Float
  inPortsType :: a -> [PortType]
  outPortsType :: a -> [PortType]
  numFirings :: a -> Int

-- is there a better utilization than weighted by operator area
utilWeightedByArea :: (SpaceTime a) => [a] -> Int
utilWeightedByArea ops = unnormalizedUtil / (fromIntegral $ length ops)
    where unnormalizedUtil = foldl (+) 0 $
            map (\op -> (fromIntegral $ opsArea $ space op) * (util op)) ops

class Composable a where
  -- This is for making ComposeSeq
  (|.|) :: Maybe a -> a -> Maybe a
  -- This is for making ComposePar
  (|&|) :: Maybe a -> a -> Maybe a

-- This is in same spirit as Monad's >>=, kinda abusing notation
-- It's |.| in reverse so that can create pipelines in right order
(|>>=|) op1 op0 = op0 (|.|) op1

canComposeSeq :: (SpaceTime a) => a -> a -> Bool
-- only join two nodes if token types match, ports do same number of tokens
-- over all firings and streams per firing, and if same number of clock cycles
canComposeSeq op0 op1 = 
  -- this checks both token types and numTokens over all firing/stream combos
  portsScaledByFirings op0 == portsScaledByFirings op1 &&
  (seqTime . time) op0 == (seqTime . time) op1
  where portsScaledByFirings op = increasePortsStreamLens (numFirings op) $ outPortsType op

canComposePar :: (SpaceTime a) => a -> a -> Bool
-- only join two nodes in parallel if same number of clocks
canComposePar op0 op1 = (seqTime . time) op0 == (seqTime . time) op1

-- this depends the constructors verifying that only composing in parallel
-- things that take same amount of time
-- should this be equal clocks and max combinational? Should this depend on stream length?
-- it should be fine to just check clocks and return max combuinational. The stream lenghts don't matter as long as total time the same.
-- can have different stream lengths as long as streams take same amount of time to finish
-- space time helpers for compose that are used for all implementations
spaceCompose :: (SpaceTime a) => [a] -> Int
spaceCompose ops = foldl (|+|) addId $ map space ops

timeComposeSeq :: (SpaceTime a) => [a] -> Int
timeComposeSeq ops = foldl (|+|) addId $ map time ops
timeComposePar :: (SpaceTime a) => [a] -> Int
timeComposePar ops = SCTime (seqTime $ time $ head ops) maxCombTime
    where maxCombTime = maximum $ map (combTime . time) ops

inPortsTypeComposeSeq :: (SpaceTime a) => [a] -> [PortType] 
-- need to replicate ports over firings
inPortsTypeComposeSeq ops = foldl (++) [] $ map inPortsType ops
inPortsTypeComposePar :: (SpaceTime a) => [a] -> [PortType] 
inPortsTypeComposePar ops = inPortsType $ head ops

outPortsTypeComposeSeq :: (SpaceTime a) => [a] -> [PortType] 
outPortsTypeComposeSeq ops = foldl (++) [] $ map outPortsType ops
outPortsTypeComposePar :: (SpaceTime a) => [a] -> [PortType] 
outPortsTypeComposePar ops = outPortsType $ last ops

-- These are leaf nodes for Op that do math
data ArithmeticOp =
  Add TokenType
  | Sub TokenType
  | Mul TokenType
  | Div TokenType
  deriving (Eq, Show)

twoInSimplePorts t = portsFromTokens [("I0", 1, 1, t), ("I1", 1, 1, t)]
oneOutSimplePort t = portsFromTokens [("O", 1, t)]

instance SpaceTime ArithmeticOp where
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

-- These are leaf nodes that do memory ops, they read and write 
-- one token
data MemoryOp = Mem_Read TokenType | Mem_Write TokenType deriving (Eq, Show)

instance SpaceTime MemoryOp where
  space (Mem_Read t) = OWA (len t) (len t)
  space (Mem_Write t) = space (Mem_Read t)
  -- assuming reads are 
  time _ = SCTime 0 rwTime
  util _ = 1.0
  inPortsType (Mem_Read _) = []
  inPortsType (Mem_Write t) = oneOutSimplePort t
  outPortsType (Mem_Read t) = portsFromTokens [("I", 1, 1, t)]
  outPortsType (Mem_Write _) = []
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
    reduceTreeSpace |+| (space op) |+| (registerSpace $ outPortsType op)
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
    increasePortsStreamLens uc (inPortsType op)
  inPortsType (ReduceSF ParParams{parallelism = p} op) = duplicatePorts p $
    increasePortsStreamLens uc (inPortsType op)
  inPortsType (ArithmeticSF op) = inPortsType op
  inPortsType (MemorySF op) = inPortsType op
  inPortsType (ComposeParSF ops) = inPortsTypeComposePar ops
  inPortsType (ComposeSeqSF ops) = inPortsTypeComposeSeq ops

  outPortsType (MapSF (ParParams p uc _) op) = duplicatePorts p $
    increasePortsStreamLens uc (outPortsType op)
  outPortsType (ReduceSF _ op) = outPortsType op
  outPortsType (ArithmeticSF op) = outPortsType op
  outPortsType (MemorySF op) = outPortsType op
  outPortsType (ComposeParSF ops) = outPortsTypeComposePar ops
  outPortsType (ComposeSeqSF ops) = outPortsTypeComposeSeq ops

  numFirings _ = 1

instance Composable SingleFiringOp where 
  (|.|) (Just op0@(ComposeSeqSF ops0)) op1@(ComposeSeqSF ops1) | canComposeSeq op0 op1 =
    Just $ ComposeSeqSF $ ops0 ++ ops1
  (|.|) (Just op0@(ComposeSeqSF ops0)) op1 | canComposeSeq op0 op1 =
    Just $ ComposeSeqSF $ ops0 ++ [op1]
  (|.|) (Just op0) op1@(ComposeSeqSF ops1) | canComposeSeq op0 op1 =
    Just $ ComposeSeqSF $ [op0] ++ ops1
  (|.|) (Just op0) op1 | canComposeSeq op0 op1 = Just $ ComposeSeqSF [op0, op1]
  (|.|) _ _ = Nothing

  (|&|) (Just op0@(ComposeParSF ops0)) op1@(ComposeParSF ops1) | canComposePar op0 op1 =
    Just $ ComposeParSF $ ops0 ++ ops1
  (|&|) (Just op0@(ComposeParSF ops0)) op1 | canComposePar op0 op1 =
    Just $ ComposeParSF $ ops0 ++ [op1]
  (|&|) (Just op0) op1@(ComposeParSF ops1) | canComposePar op0 op1 =
    Just $ ComposeParSF $ [op0] ++ ops1
  (|&|) (Just op0) op1 | canComposePar op0 op1 = Just $ ComposeParSF [op0, op1]
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
  util (ComposeParSF ops) = utilWeightedByArea ops
  util (ComposeSeqSF ops) = utilWeightedByArea ops

  inPortsType (Iter _ op) = inPortsType op
  inPortsType (ComposeParMF ops) = inPortsTypeComposePar ops
  inPortsType (ComposeSeqMF ops) = inPortsTypeComposeSeq ops

  outPortsType (Iter _ op) = outPortsType op
  outPortsType (ComposeParMF ops) = outPortsTypeComposePar ops
  outPortsType (ComposeSeqMF ops) = outPortsTypeComposeSeq ops

  numFirings (Iter (IterParams n) (Left op)) = n * (numFirings op)
  numFirings (Iter (IterParams n) (Right op)) = n * (numFirings op)

data Schedule1D = S_1D [MultipleFiringOp]

instance SpaceTime Schedule1D where
  space (S_1D ops) = foldl (|+|) owAreaZero $ map space ops
  -- TODO: make this account for pipelining
  -- all schedules in parallel must take same time, constructor will 
  -- require this
  time (S_1D ops) = foldl (|+|) scTimeZero $ map time ops
  inPortsType (S_1D ops) = inPortsType $ head ops
  outPortsType (S_1D ops) = outPortsType $ last ops
  -- TODO: This assumes a S_1D is a unit that other schedules can interface
  -- with through ready-valid but not timing. Is that right?
  streamLens (S_1D ops) = IOSLens (iIn * nIn) (oOut * nOut) 1
    where (IOSLens iIn _ nIn) = streamLens $ head ops
          (IOSLens _ oOut nOut) = streamLens $ last ops
  -- is there a better utilization than weighted by operator area
  util (S_1D ops) =  unnormalizedUtil / (fromIntegral $ length ops)
    where unnormalizedUtil = foldl (+) 0 $ 
            map (\op -> (fromIntegral $ opsArea $ space op) * (util op)) ops

data Schedule2D = S_2D [[Schedule1D]]

-- TODO: A constructor with right precedence so can be used with |.| as 
-- sc Iter MapSF Add |.| sc Iter MapSF ReduceSF Add

getSchedule1DOps :: Schedule1D -> [MultipleFiringOp]
getSchedule1DOps (S_1D ops) = ops

instance SpaceTime Schedule2D where
  space (S_2D opss) = sumSpace $ sumSpace $ map (map space) opss
    where sumSpace = foldl (|+|) owAreaZero 
  -- TODO: make this account for pipelining
  -- all inner shcedules run in parallel, so only need time for one
  time (S_2D opss) = sumTime $ sumTime $ map (time $ head) opss
    where sumTime = foldl (|+|) scTimeZero
  inPortsType (S_2D opss) = foldl (|+|) addId $ map inPortsType $ head opss
  outPortsType (S_2D opss) = foldl (|+|) addId $ map inPortsType $ tail opss
  -- TODO: This assumes a Schedule is a unit that other schedules can interface
  -- with through ready-valid but not timing. Is that right?
  streamLens (S_2D opss) = IOSLens (iIn * nIn) (oOut * nOut) 1
    where (IOSLens iIn _ nIn) = streamLens $ head ops
          (IOSLens _ oOut nOut) = streamLens $ last ops
  -- is there a better utilization than weighted by operator area
  util (Schedule ops) =  unnormalizedUtil / (fromIntegral $ length ops)
    where unnormalizedUtil = foldl (+) 0 $ 
            map (\op -> (fromIntegral $ opsArea $ space op) * (util op)) ops

-- For creating the compose ops
(|.|) :: Maybe Schedule -> Maybe Schedule -> Maybe Schedule
(|.|) (Just s0@(Schedule ops0)) (Just s1@(Schedule ops1))
  -- only join two schedules if same stream lengths over total firings
  -- output port types of first are input port types of second
  -- and same average number of tokens per clock
  | outNumTokens ops0tl == inNumTokens ops1hd && 
    outPortsType ops0tl == inPortsType ops1hd &&
    (seqTime $ time s0) == (seqTime $ time s1) = Just $ Schedule $ ops0 ++ ops1
  where
    ops0tl = last ops0
    ops1hd = head ops1
(|.|) _ _ = Nothing

-- This is in same spirit as Monad's >>=, kinda abusing notation
(|>>=|) op1 op0 = op0 (|.|) op1
