-- constant for lengths
counterLen = 1

-- the typeclasses that all the elements of the IR must implement

class HasStreamLen a =
  streamLen :: a -> Int 

class SpaceTime a =
  spaceOps :: a -> Int
  spaceWire :: a -> Int
  time :: a -> Int

-- data SpaceTimeOp = 
--   MapSeqST MapSeq
--   | MapParST MapPar
--   | MapDelayST MapDelay
--   | Ma

-- the elements of the type system

-- MapSeq handles mapping >= 0 in sequence dimension
-- min length is 0 and there is no max
data MapSeq a = MapSeq {
  seqStreamLen :: Int,
  seqContainedOp :: a
}

-- MapPar handles mapping >= 0 in token dimension
-- The tokens to MapPar are arrays of length parallelism of tokens for 
-- parContainedOp
data MapPar a = MapPar {
  parallelism :: Int,
  parContainedOp :: a
}

instance (HasStreamLen a) => StreamLen (MapPar a) where
  streamLen MapPar {op = parContainedOp} = streamLen op

-- MapDelay hanndles mapping <= 0 in token dimension
-- The tokens for mapDelay are the same as the tokens for delayContainedOp
-- This emits a token numeratorActive / denominatorActive clocks
data MapDelay a = MapDelay {
  numeratorActive :: Int,
  denominatorActive :: Int,
  delayContainedOp :: a
}

areaOps :: SpaceTime -> Int
areaWiring :: SpaceTime -> Int
time :: SpaceTime -> Int
s SpaceTime a where
  areaOps :: a -> int
  areaWiring :: a -> int
  time :: a -> int

data Map = MapSeq int 
