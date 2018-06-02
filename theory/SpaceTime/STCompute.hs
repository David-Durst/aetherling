module SpaceTime.STCompute where
import SpaceTime.STHelpers
-- the typeclasses that all the elements of the IR must implement

class SpaceTime a where
  space :: a -> OpsWireArea
  -- this is the time to process one or more firings
  time :: a -> SeqCombTime
  inTokenType :: a -> TokenType
  outTokenType :: a -> TokenType
  StreamLens :: a -> IOStreamLens

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
  streamLens _ = IOSLens 1 1 0


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
  outTokenType (Mem_Write _) = T_Unit
  streamLens (Mem_Read _) = IOSLens 0 1 0
  streamLens (Mem_Write _) = IOSLens 1 0 0

-- ParParams handles parallelism and inverse parallelism (aka underutilization)
-- for a single firing
-- utilizedClocks / allClocksInStream = pct of a firing that this op is utilized
data ParParams = ParParams { parallelism :: Int, utilizedClocks :: Int,
  allClocksInStream :: Int }

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
  space (Map (ParParams {p = parallelism}) op) = (space op) |* p
  -- area of reduce is area of reduce tree, with area for register for partial
  -- results if a signle firing is more than 1 token
  space rp@(Reduce (ParParams {p = parallelism}) op) =
    if streamLen rp > 1 
      -- add 1 op and regster as need register for partial result and need op 
      -- to combine reduceTree results with that register if stream more than 1
      -- clock
      then reduceTreeSpace |+| (space op) |+| (registerSpace $ tType op)
      else reduceTreeSpace
    where reduceTreeSpace = (space op) |* (p-1)
  space (Arithmetic op) = space op
  space (Memory op) = space op

  time (Map _ op) = (time op) -- need to handle stream length in here
  time (Reduce _ op) = 
    if streamLen rp > 1 
      -- add 1 op and register for same reason as above 
      then reduceTreeTime |+| (time op) |+| registerTime
      else reduceTreeTime
    where reduceTreeTime = (Time op) |* (ceilLog p)
  time (Arithmetic op) = time op
  time (Memory op) = time op

  inTokenType (Map (ParParams {p = parallelism}) op) = 
    arrayTokenBuilder (inTokenType op) p
  outTokenType (Map (ParParams {p = parallelism}) op) = 
    arrayTokenBuilder (outTokenType op) p
  inTokenType (Reduce (ParParams {p = parallelism}) op) = 
    arrayTokenBuilder (inTokenType op) p
  outTokenType (Reduce _ op) = outTokenType op
  inTokenType (Arithmetic op) = inTokenType op
  outTokenType (Arithmetic op) = outTokenType op
  inTokenType (Memory op) = inTokenType op
  outTokenType (Memory op) = outTokenType op

data MultipleFiringOp = MapSeq MapSeqParams (Either MultipleFiringOp SingleFiringOp)

instance SpaceTime MultipleFiringOp where
  -- when mapping over sequence, area is time to count over sequence plus 
  -- area of stuff that is being applied to each element of sequence
  space (MapSeq (MapSeqParams {s = seqStreamLen}) (Left op)) = 
    (counterSpace s) |+| (space op)
  space (MapSeq (MapSeqParams {s = seqStreamLen}) (Right op)) = 
    (counterSpace s) |+| (space op)
  -- if this is a combinational node, add clocks to it equal to num cycles
  -- going to run over
  -- otherwise just multiply it by stream length as registers going to increase
  -- with |* operator
  time (MapSeq (MapSeqParams {s = seqStreamLen}) (Left op)) =
    replicateTimeOverStream (time op) s
  time (MapSeq (MapSeqParams {s = seqStreamLen}) (Right op)) =
    replicateTimeOverStream (time op) s
  inTokenType (MapSeq _ (Left op)) = inTokenType op
  outTokenType (MapSeq _ (Left op)) = outTokenType op
  inTokenType (MapSeq _ (Right op)) = inTokenType op
  outTokenType (MapSeq _ (Right op)) = outTokenType op

data Schedule =
  ComposeWrapper MultipleFiringOp
  | Compose ComposeWrapper ComposeWrapper

instance SpaceTime MultipleOps where
  space (Compose op0 op1) = (space op0) |+| (space op1)
  space (ComposeWrapper op) = space op
  time (Compose op0 op1) = (time op0) |+| (time op1)
  time (ComposeWrapper op) = time op
  inTokenType (Compose op0 _) = inTokenType op0
  inTokenType (ComposeWrapper op) = inTokenType op
  outTokenType (Compose _ op1) = outTokenType op1
  outTokenType (ComposeWrapper op) = outTokenType op

-- For creating the compose ops
(|.|) :: Maybe MultipleOps -> Maybe MultipleOps -> Maybe MultipleOps
(|.|) (Just op0) (Just op1) | outStreamLen(op0) == inStreamLen(op1) &&
  outTokenType(op0) == inTokenType(op1) = Just (Combine op0 op1)
(|.|) _ _ = Nothing

-- This is in same spirit as Monad's >>=, kinda abusing notation
(|>>=|) :: Maybe MultipleOps -> Maybe MultipleOps -> Maybe MultipleOps
(|>>=|) op1 op0 = op0 (|.|) op1

-- MapSeq handles mapping in multiple firing dimension
-- min length is 0 and there is no max
data IterParams = InterParams { seqStreamLen :: Int }

