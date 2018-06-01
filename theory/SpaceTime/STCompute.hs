module SpaceTime.STCompute where
import SpaceTime.STHelpers
-- the typeclasses that all the elements of the IR must implement

class SpaceTime a where
  space :: a -> OpsWireArea
  -- this is the time to process one token, not a full stream of them
  time :: a -> SeqCombTime
  inTokenType :: a -> TokenType
  outTokenType :: a -> TokenType
  inStreamLen :: a -> Int
  outStreamLen :: a -> Int

-- all leaf ops operate over streams of length one

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
  inTokenType (Add t) = t
  outTokenType (Add t) = t
  inTokenType (Sub t) = t
  outTokenType (Sub t) = t
  inTokenType (Mul t) = t
  outTokenType (Mul t) = t
  inTokenType (Div t) = t
  outTokenType (Div t) = t
  inStreamLen _ = 1
  outStreamLen _ = 1


-- These are leaf nodes that do memory ops, they read and write 
-- one token
data MemoryOp = Mem_Read TokenType | Mem_Write TokenType deriving (Eq, Show)

instance SpaceTime MemoryOp where
  space (Mem_Read t) = OWA (len t) (len t)
  space (Mem_Write t) = space (Mem_Read t)
  -- assuming reads are 
  time _ = SCTime 0 rwTime
  inTokenType (Mem_Read _) = T_Unit
  outTokenType (Mem_Read t) = t
  inTokenType (Mem_Write t) = t
  outTokenType (Mem_Write t) = T_Unit
  streamLen _ = 1

-- leaf nodes define what is done for one token in one firing, and 
-- can scale them up or down across one firing and multiple firings
--
-- These are the scheduling ops for building types of pipelines in the lower, 
-- scheduled IR. 
-- Can schedule in two dimenions - over multiple firings (for handling more tokens)
-- and a single firing (for doing wider or narrower tokens over different
-- numbers of clocks)
-- mapSeq handles scaling all operations over multiple firings
-- mapPar, reducePar, and foldPar handle scaling operations in single firing
-- Must handle 0 or more tokens so the min of stream dimension is 0
-- Must handle 0 or more tokens per clock so the min of token dimension is 0
data SchedulingOp = 
  Compose SchedulingOp SchedulingOp
  | MapSeq MapSeqParams SchedulingOp
  | MapPar ParParams SchedulingOp
  | ReducePar ParParams SchedulingOp
  | Arithmetic ArithmeticOp
  | Memory MemoryOp

instance SpaceTime SchedulingOp where
  space (Compose op0 op1) = (space op0) |+| (space op1)
  -- when mapping over sequence, area is time to count over sequence plus 
  -- area of stuff that is being applied to each element of sequence
  space (MapSeq (MapSeqParams {s = seqStreamLen}) op) = 
    (counterSpace s) |+| (space op)
  -- area of parallel map is area of all the copies
  space (MapPar (ParParms {p = parallelism}) op) = (space op) |* p
  -- area of reduce is area of reduce tree, with area for register for partial
  -- results if a signle firing is more than 1 token
  space rp@(ReducePar (ParParams {p = parallelism}) op) =
    if streamLen rp > 1 
      -- add 1 op and regster as need register for partial result and need op 
      -- to combine reduceTree results with that register if stream more than 1
      -- clock
      then reduceTreeSpace |+| (space op) |+| (registerSpace $ tType op)
      else reduceTreeSpace
    where reduceTreeSpace = (space op) |* (p-1)
  space (Arithmetic op) = space op
  space (Memory op) = space op
  time (Compose op0 op1) = (time op0) |+| (time op1)
  time (MapSeq _ op) = (time op) 
  time (MapPar _ op) = (time op)
  time (ReducePar _ op) = 
    if streamLen rp > 1 
      -- add 1 op and register for same reason as above 
      then reduceTreeTime |+| (time op) |+| registerTime
      else reduceTreeTime
    where reduceTreeTime = (Time op) |* (ceilLog p)
  time (Arithmetic op) = time op
  time (Memory op) = time op

-- For creating the compose ops
(|.|) :: Maybe SchedulingOp -> Maybe SchedulingOp -> Maybe SchedulingOp
(|.|) (Just op0) (Just op1) | streamLen(op0) == streamLen(op1) &&
  tType(op0) == tType(op1) = Just (Combine op0 op1)
(|.|) _ _ = Nothing

-- This is in same spirit as Monad's >>=, kinda abusing notation
(|>>=|) :: Maybe SchedulingOp -> Maybe SchedulingOp -> Maybe SchedulingOp
(|>>=|) op1 op0 = op0 (|.|) op1

-- MapSeq handles mapping in multiple firing dimension
-- min length is 0 and there is no max
data MapSeqParams = MapSeqParams { seqStreamLen :: Int }

-- ParParams handles mapping and reducing in single firing dimension
-- The tokens to ReducePar and MapPar are arrays of length parallelism of tokens 
-- for the contained op, unless parallelism is 1, then they are the same as the 
-- tokens for the contained op
-- utilizedClocks / allClocksInStream = pct of a firing that this op is utilized
data ParParams = MapParParams { parallelism :: Int, utilizedClocks :: Int,
  allClocksInStream :: Int }

instance (HasStreamLen a) => StreamLen (MapPar a) where
  streamLen MapPar {op = parContainedOp} = streamLen op

areaOps :: SpaceTime -> Int
areaWiring :: SpaceTime -> Int
time :: SpaceTime -> Int
s SpaceTime a where
  areaOps :: a -> int
  areaWiring :: a -> int
  time :: a -> int

data Map = MapSeq int 
