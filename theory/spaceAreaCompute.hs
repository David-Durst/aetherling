-- constant for lengths
counterLen = 1

-- constants for scaling mul and div compared to add and sub
mulSpaceTimeIncreaser = 5
divSpaceTimeIncreaser = 5

-- the types used for tracking data about operators
data TokenType =
  T_Int
  | T_Bit
  | T_Array TokenType Int -- Int here is the length
  deriving (Eq)

len :: TokenType -> Int
len T_Int = 8
len T_Bit = 1
len (T_Array t i) = i * len t

-- first int tracks max combinational path time, second tracks number of cycles
data SeqCombTime = SCTime Int Int deriving (Eq, Show)

addTimes :: SeqCombTime -> SeqCombTime -> SeqCombTime
-- if either is just a combinational element, combinational time increases
-- and num cycles is constant
addTimes (SCTime c1 s1) (SCTime c2 s2) | s1 == 0 || s2 == 0 = 
  SCTime (c1 + c2) (max s1 s2)
-- if both are sequential, assume registers at end of each op
addTimes (SCTime c1 s1) (SCTime c2 s2) | s1 == 0 && s2 == 0 = 
  SCTime (max c1 c2) (s1 + s2)

-- the typeclasses that all the elements of the IR must implement

class HasStreamLen a =

class SpaceTime a =
  spaceOps :: a -> Int
  spaceWire :: a -> Int
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
  deriving (HasSpaceTime, Eq)

instance SpaceTime ArithmeticOp where
  spaceOps (Add t) = 1 * len t
  spaceOps (Sub t) = 1 * len t
  spaceOps (Mul t) = mulSpaceTimeIncreaser * len t
  spaceOps (Div t) = divSpaceTimeIncreaser * len t
  spaceWire (Add t) = 2 (len t)
  spaceWire (Sub t) = spaceWire (Add t)
  spaceWire (Mul t) = spaceWire (Add t)
  spaceWire (Div t) = spaceWire (Add t)
  time (Add t) = SCTime 1 0
  time (Sub t) = SCTime 1 0
  time (Mul t) = SCTime mulSpaceTimeIncreaser 0
  time (Div t) = SCTime divSpaceTimeIncreaser 0
  tType (Add t) = t
  tType (Sub t) = tType (Add t)
  tType (Mul t) = tType (Add t)
  tType (Div t) = tType (Add t)
  streamLen _ = 1


-- These are leaf nodes that do memory ops, they read and write 
-- one token 
data MemoryOp = Mem_Read TokenType | Mem_Write TokenType deriving (HasStreamLen,
  HasTokenType, HasSpaceTime)


-- These are the scheduling ops for building types of pipelines in the lower, 
-- scheduled IR. 
-- Can schedule in two dimenions - over multiple firings (for handling more tokens)
-- and a single firing (for doing wider or narrower tokens over different
-- numbers of clocks)
-- Must handle 0 or more tokens so the min of stream dimension is 0
-- Must handle 0 or more tokens per clock so the min of token dimension is 0
data SchedulingOp = 
  Compose SchedulingOp SchedulingOp
  | MapSeq MapSeqParams SchedulingOp
  | MapPar MapParParams SchedulingOp
  | ReducePar ReduceParParams SchedulingOp
  | Arithmetic ArithmeticOp

(c) :: Maybe SchedulingOp -> Maybe SchedulingOp -> Maybe SchedulingOp
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
