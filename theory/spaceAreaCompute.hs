-- constant for lengths
counterLen = 1

-- the typeclasses that all the elements of the IR must implement

class HasStreamLen a =
  streamLen :: a -> Int 

class SpaceTime a =
  spaceOps :: a -> Int
  spaceWire :: a -> Int
  time :: a -> Int

-- These are leaf nodes for Op that do math
data ArithmeticOp = Add | Sub | Mul | Div

-- These are the scheduling ops for building types of pipelines in the lower, 
-- scheduled IR. 
-- Can schedule in two dimenions - over streams (for handling more data)
-- and over parallelism (for doing more or fewer tokens at once)
data SchedulingOp = 
  MapSeq MapSeqParams SchedulingOp
  | MapPar MapParParams SchedulingOp
  | MapDelay MapDelayParams SchedulingOp
  | ReducePar ReduceParParams SchedulingOp
  | ReduceDelay ReduceDelayParams SchedulingOp
  | Arithmetic ArithmeticOp


-- MapSeq handles mapping >= 0 in sequence dimension
-- min length is 0 and there is no max
data MapSeqParams {
  seqStreamLen :: Int,
}

-- MapPar handles mapping >= 0 in token dimension
-- The tokens to MapPar are arrays of length parallelism of tokens for 
-- parContainedOp
data MapParams a = MapPar { parallelism :: Int }

instance (HasStreamLen a) => StreamLen (MapPar a) where
  streamLen MapPar {op = parContainedOp} = streamLen op

-- MapDelay hanndles mapping <= 0 in token dimension
-- The tokens for mapDelay are the same as the tokens for delayContainedOp
-- This emits a token numeratorActive / denominatorActive clocks
data MapDelayParams = MapDelay { numeratorActive :: Int, denominatorActive :: Int }

areaOps :: SpaceTime -> Int
areaWiring :: SpaceTime -> Int
time :: SpaceTime -> Int
s SpaceTime a where
  areaOps :: a -> int
  areaWiring :: a -> int
  time :: a -> int

data Map = MapSeq int 
