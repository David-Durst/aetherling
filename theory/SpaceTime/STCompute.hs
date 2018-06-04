module SpaceTime.STCompute where
import SpaceTime.STHelpers
-- the typeclasses that all the elements of the IR must implement

class SpaceTime a where
  space :: a -> OpsWireArea
  -- this is the time to process one or more firings
  time :: a -> SeqCombTime
  util :: a -> Float
  inPortsType :: a -> PortsType
  outPortsType :: a -> PortsType
  streamLens :: a -> IOStreamLens

inNumTokens :: (SpaceTime a) => a -> Int
inNumTokens op = tokensInOneFiring * n
  where (IOSLens tokensInOneFiring _ n) = streamLens op
outNumTokens :: (SpaceTime a) => a -> Int
outNumTokens op = tokensOutOneFiring * n
  where (IOSLens _ tokensOutOneFiring n) = streamLens op

-- These are leaf nodes for Op that do math
data ArithmeticOp =
  Add TokenType
  | Sub TokenType
  | Mul TokenType
  | Div TokenType
  deriving (Eq, Show)

twoInPorts t = portsFromTokens [("I0", 1, t), ("I1", 1, t)]
oneOutPort t = portsFromTokens [("O", 1, t)]

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
  inPortsType (Add t) = twoInPorts t
  inPortsType (Sub t) = twoInPorts t
  inPortsType (Mul t) = twoInPorts t
  inPortsType (Div t) = twoInPorts t
  outPortsType (Add t) = oneOutPort t
  outPortsType (Sub t) = oneOutPort t
  outPortsType (Mul t) = oneOutPort t
  outPortsType (Div t) = oneOutPort t
  streamLens _ = IOSLens 1 1 0

-- These are leaf nodes that do memory ops, they read and write 
-- one token
data MemoryOp = Mem_Read TokenType | Mem_Write TokenType deriving (Eq, Show)

instance SpaceTime MemoryOp where
  space (Mem_Read t) = OWA (len t) (len t)
  space (Mem_Write t) = space (Mem_Read t)
  -- assuming reads are 
  time _ = SCTime 0 rwTime
  util _ = 1.0
  inPortsType (Mem_Read _) = T_Ports []
  inPortsType (Mem_Write t) = oneOutPort t
  outPortsType (Mem_Read t) = portsFromTokens [("I", 1, t)]
  outPortsType (Mem_Write _) = T_Ports []
  streamLens (Mem_Read _) = IOSLens 0 1 0
  streamLens (Mem_Write _) = IOSLens 1 0 0

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

data SingleFiringOp = 
  MapSF ParParams SingleFiringOp
  | ReduceSF ParParams SingleFiringOp
  | ArithmeticSF ArithmeticOp
  | MemorySF MemoryOp
  deriving (Eq, Show)

instance SpaceTime SingleFiringOp where
  -- area of parallel map is area of all the copies
  space (MapSF ParParams{parallelism = p} op) = (space op) |* p
  -- area of reduce is area of reduce tree, with area for register for partial
  -- results if a signle firing is more than 1 clock
  space (ReduceSF ParParams{utilizedClocks = uc, parallelism = p} op) =
    if uc > 1 
      -- add 1 op and regster as need register for partial result and need op 
      -- to combine reduceTree results with that register if stream more than 1
      -- clock
      then reduceTreeSpace |+| (space op) |+| (registerSpace $ outPortsType op)
      else reduceTreeSpace
    where reduceTreeSpace = (space op) |* (p-1)
  space (ArithmeticSF op) = space op
  space (MemorySF op) = space op

  time (MapSF ParParams{allClocksInStream = ac} op) =
    replicateTimeOverStream (time op) ac
  time (ReduceSF (ParParams p uc ac) op) = 
    if ac > 1 
      -- add 1 op and register for same reason as above 
      then replicateTimeOverStream (reduceTreeTime |+| (time op) |+| registerTime) ac
      else replicateTimeOverStream reduceTreeTime ac
    where reduceTreeTime = (time op) |* (ceilLog p)
  time (ArithmeticSF op) = time op
  time (MemorySF op) = time op

  util (MapSF (ParParams _ uc ac) op) = (util op) * (fromIntegral uc) / (fromIntegral ac)
  util (ReduceSF (ParParams _ uc ac) op) = (util op) * (fromIntegral uc) / (fromIntegral ac)
  util (ArithmeticSF op) = util op
  util (MemorySF op) = util op

  inPortsType (MapSF ParParams{parallelism = p} op) = (inPortsType op) |* p
  inPortsType (ReduceSF ParParams{parallelism = p} op) = (inPortsType op) |* p
  inPortsType (ArithmeticSF op) = inPortsType op
  inPortsType (MemorySF op) = inPortsType op
  outPortsType (MapSF ParParams{parallelism = p} op) = (outPortsType op) |* p
  outPortsType (ReduceSF _ op) = outPortsType op
  outPortsType (ArithmeticSF op) = outPortsType op
  outPortsType (MemorySF op) = outPortsType op

  -- number of firings should be 0 as not wrapped in multiple firings at this 
  -- point
  streamLens (MapSF ParParams{utilizedClocks = uc} op) = IOSLens (i * uc) (o * uc) 0
    where (IOSLens i o _) = streamLens op
  streamLens (ReduceSF ParParams{utilizedClocks = uc} op) = IOSLens (i * uc) o 0
    where (IOSLens i o _) = streamLens op
  streamLens (ArithmeticSF op) = streamLens op
  streamLens (MemorySF op) = streamLens op

-- Iter handles mapping in multiple firing dimension
-- min length is 0 and there is no max
data IterParams = IterParams { numIterations :: Int } deriving (Eq, Show)

data MultipleFiringOp = Iter IterParams (Either MultipleFiringOp SingleFiringOp)
  deriving (Eq, Show)

instance SpaceTime MultipleFiringOp where
  -- when mapping over sequence, area is time to count over sequence plus 
  -- area of stuff that is being applied to each element of sequence
  space (Iter (IterParams numIters) (Left op)) = 
    (counterSpace numIters) |+| (space op)
  space (Iter (IterParams numIters) (Right op)) = 
    (counterSpace numIters) |+| (space op)

  time (Iter (IterParams numIters) (Left op)) =
    replicateTimeOverStream (time op) numIters
  time (Iter (IterParams numIters) (Right op)) =
    replicateTimeOverStream (time op) numIters

  util (Iter _ (Left op)) = util op
  util (Iter _ (Right op)) = util op

  inPortsType (Iter _ (Left op)) = inPortsType op
  inPortsType (Iter _ (Right op)) = inPortsType op
  outPortsType (Iter _ (Left op)) = outPortsType op
  outPortsType (Iter _ (Right op)) = outPortsType op

  streamLens (Iter (IterParams numIters) (Left op)) =
    IOSLens i o ((max n 1) * numIters)
    where (IOSLens i o n) = streamLens op
  streamLens (Iter (IterParams numIters) (Right op)) =
    IOSLens i o ((max n 1) * numIters)
    where (IOSLens i o n) = streamLens op

data Schedule1D = S_1D [MultipleFiringOp]

instance SpaceTime Schedule where
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

getScheduleOps :: Schedule -> [MultipleFiringOp]
getScheduleOps (Schedule ops) = ops

instance SpaceTime Schedule2D where
  space (S_2D opss) = sumSpace $ sumSpace $ map (map space) opss
    where sumSpace = fold (|+|) owAreaZero 
  -- TODO: make this account for pipelining
  time (S_2D opss) = sumTime $ sumTime $ map (map time) opss
    where sumTime = fold (|+|) scTimeZero
  -- all schedules in parallel must take same time, constructor will 
  -- require this
  time (S_ND sPar sNext) = time sPar |+| 
  time (Schedule ops) = foldl (|+|) scTimeZero $ map time ops
  inPortsType (Schedule ops) = inPortsType $ head ops
  outPortsType (Schedule ops) = outPortsType $ last ops
  -- TODO: This assumes a Schedule is a unit that other schedules can interface
  -- with through ready-valid but not timing. Is that right?
  streamLens (Schedule ops) = IOSLens (iIn * nIn) (oOut * nOut) 1
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
