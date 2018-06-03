module SpaceTime.STCompute where
import SpaceTime.STHelpers
-- the typeclasses that all the elements of the IR must implement

class SpaceTime a where
  space :: a -> OpsWireArea
  -- this is the time to process one or more firings
  time :: a -> SeqCombTime
  util :: a -> Float
  inTokenType :: a -> TokenType
  outTokenType :: a -> TokenType
  streamLens :: a -> IOStreamLens

inNumTokens :: (SpaceTime a) => a -> Int
inNumTokens (IOSLens tokensInOneFiring _ n) = tokensInOneFiring * n
outNumTokens :: (SpaceTime a) => a -> Int
outNumTokens (IOSLens _ tokensOutOneFiring n) = tokensOutOneFiring * n

-- These are leaf nodes for Op that do math
data ArithmeticOp =
  Add TokenType
  | Sub TokenType
  | Mul TokenType
  | Div TokenType
  deriving (Eq, Show)

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
  inTokenType (Add t) = t
  inTokenType (Sub t) = t
  inTokenType (Mul t) = t
  inTokenType (Div t) = t
  outTokenType (Add t) = t
  outTokenType (Sub t) = t
  outTokenType (Mul t) = t
  outTokenType (Div t) = t
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
  inTokenType (Mem_Read _) = T_Unit
  inTokenType (Mem_Write t) = t
  outTokenType (Mem_Read t) = t
  outTokenType (Mem_Write _) = T_Unit
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
  Map ParParams SingleFiringOp
  | Reduce ParParams SingleFiringOp
  | Arithmetic ArithmeticOp
  | Memory MemoryOp
  deriving (Eq, Show)

instance SpaceTime SingleFiringOp where
  -- area of parallel map is area of all the copies
  space (Map ParParams{parallelism = p} op) = (space op) |* p
  -- area of reduce is area of reduce tree, with area for register for partial
  -- results if a signle firing is more than 1 token
  space rp@(Reduce ParParams{parallelism = p} op) =
    if streamLen rp > 1 
      -- add 1 op and regster as need register for partial result and need op 
      -- to combine reduceTree results with that register if stream more than 1
      -- clock
      then reduceTreeSpace |+| (space op) |+| (registerSpace $ tType op)
      else reduceTreeSpace
    where reduceTreeSpace = (space op) |* (p-1)
  space (Arithmetic op) = space op
  space (Memory op) = space op

  time (Map ParParams{allClocksInStream = ac} op) =
    replicateTimeOverStream (time op) ac
  time (Reduce ParParams{allClocksInStream = ac} op) = 
    if streamLen rp > 1 
      -- add 1 op and register for same reason as above 
      then replicateTimeOverStream (reduceTreeTime |+| (time op) |+| registerTime) ac
      else replicateTimeOverStream reduceTreeTime
    where reduceTreeTime = (Time op) |* (ceilLog p)
  time (Arithmetic op) = time op
  time (Memory op) = time op

  inTokenType (Map ParParams{parallelism = p} op) = 
    arrayTokenBuilder (inTokenType op) p
  inTokenType (Reduce ParParams{parallelism = p} op) = 
    arrayTokenBuilder (inTokenType op) p
  inTokenType (Arithmetic op) = inTokenType op
  inTokenType (Memory op) = inTokenType op
  outTokenType (Map ParParams{parallelism = p} op) = 
    arrayTokenBuilder (outTokenType op) p
  outTokenType (Reduce _ op) = outTokenType op
  outTokenType (Arithmetic op) = outTokenType op
  outTokenType (Memory op) = outTokenType op

  streamLens (Map ParParams{utilizedClocks = u} op) = IOSLens (i * u) (i * o) n
    where (IOSLens i o n) = streamLens op
  streamLens (Reduce ParParams{utilizedClocks = u} op) = IOSLens (i * u) o n
    where (IOSLens i o n) = streamLens op
  streamLens (Arithmetic op) = streamLens op
  streamLens (Memory op) = streamLens op

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

  inTokenType (Iter _ (Left op)) = inTokenType op
  inTokenType (Iter _ (Right op)) = inTokenType op
  outTokenType (Iter _ (Left op)) = outTokenType op
  outTokenType (Iter _ (Right op)) = outTokenType op

  streamLens (Iter (IterParams numIters) (Left op)) =
    IOSLens i o ((max n 1) * numIters)
    where (IOSLens i o n) = streamLens op
  streamLens (Iter (IterParams numIters) (Right op)) =
    IOSLens i o ((max n 1) * numIters)
    where (IOSLens i o n) = streamLens op

data Schedule = Schedule [MultipleFiringOp] deriving (Eq, Show)

-- TODO: A constructor with right precedence so can be used with |.| as 
-- sc Iter Map Add |.| sc Iter Map Reduce Add

getScheduleOps :: Schedule -> [MultipleFiringOp]
getScheduleOps (Schedule ops) = ops

instance SpaceTime Schedule where
  space (Schedule ops) = foldl (|+|) owAreaZero $ map space ops
  -- TODO: make this account for pipelining
  time (Schedule ops) = foldl (|+|) scTimeZero $ map time ops
  inTokenType (Schedule (opHd:_)) = inTokenType opHd
  outTokenType (Schedule ops) = outTokenType $ tail ops
  -- TODO: This assumes a Schedule is a unit that other schedules can interface
  -- with through ready-valid but not timing. Is that right?
  streamLens (Schedule ops) = IOSLens (iIn * nIn) (oOut * nOut) 1
    where (IOSLens iIn _ nIn) = streamLens $ head ops
          (IOSLens _ oOut nOut) = streamLens $ tail opTl
  -- is there a better utilization than weighted by area average?
  util (Schedule ops) =  unnormalizedUtil / (length ops)
    where unnormalizedUtil = foldl (+) 0 $ map (\op -> op * (space op)) ops

-- For creating the compose ops
(|.|) :: Maybe Schedule -> Maybe Schedule -> Maybe Schedule
(|.|) (Just ops0) (Just ops1) | outNumTokens ops0tl == inNumTokens ops1hd &&
  outTokenType ops0tl == inTokenType ops1hd = Just $ Schedule $ ops0 ++ ops1
  where
    ops0tl = tail ops0
    ops1hd = head ops1
(|.|) _ _ = Nothing

-- This is in same spirit as Monad's >>=, kinda abusing notation
(|>>=|) op1 op0 = op0 (|.|) op1
