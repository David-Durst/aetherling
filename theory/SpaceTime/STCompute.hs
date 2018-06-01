module SpaceTime.STCompute where
import SpaceTime.STHelpers
-- the typeclasses that all the elements of the IR must implement

class SpaceTime a where
  space :: a -> OpsWireArea
  time :: a -> SeqCombTime
  tType :: a -> TokenType
  streamLen :: a -> Int

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
  tType (Add t) = t
  tType (Sub t) = tType (Add t)
  tType (Mul t) = tType (Add t)
  tType (Div t) = tType (Add t)
  streamLen _ = 1


-- These are leaf nodes that do memory ops, they read and write 
-- one token
data MemoryOp = Mem_Read TokenType | Mem_Write TokenType deriving (Eq, Show)

instance SpaceTime MemoryOp where
  space (Mem_Read t) = OWA (len t) (len t)
  space (Mem_Write t) = space (Mem_Read t)
  -- assuming reads are 
  time _ = SCTime 0 rwTime
  tType (Mem_Read t) = t
  tType (Mem_Write t) = tType (Mem_Read t)
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
  | MapPar MapParParams SchedulingOp
  | ReducePar ReduceParParams SchedulingOp
  | Arithmetic ArithmeticOp

instance SpaceTime SchedulingOp where
  space (Compose op0 op1) = (space op0) |+| (space op1)
  space ms@(MapSeq _ op) = counterSpace (streamLen ms) |+| (space op)
  space (MapPar params op) = 

(|.|) :: Maybe SchedulingOp -> Maybe SchedulingOp -> Maybe SchedulingOp
(|.|) Nothing _ = Nothing
(|.|) _ Nothing = Nothing
(|.|) (Just op0) (Just op1) | 
(c) (Just op0) (Just op1) | 


-- MapSeq handles mapping >= 0 in sequence dimension
-- min length is 0 and there is no max
data MapSeqParams {
  seqStreamLen :: Int,
}

-- MapPar handles mapping >= 1 in token dimension
-- The tokens to MapPar are arrays of length parallelism of tokens for 
-- parContainedOp
data MapParParams a = MapPar { parallelism :: Int }

instance (HasStreamLen a) => StreamLen (MapPar a) where
  streamLen MapPar {op = parContainedOp} = streamLen op

-- MapDelay hanndles mapping <= 1 in token dimension
-- The tokens for mapDelay are the same as the tokens for delayContainedOp
-- This emits a token numeratorActive / denominatorActive clocks
data MapDelayParams = MapDelay { numeratorActive :: Int, denominatorActive :: Int }

data ReducePar

areaOps :: SpaceTime -> Int
areaWiring :: SpaceTime -> Int
time :: SpaceTime -> Int
s SpaceTime a where
  areaOps :: a -> int
  areaWiring :: a -> int
  time :: a -> int

data Map = MapSeq int 
