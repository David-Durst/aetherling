-- constant use for scaling operations in space and time
RWtime = 1
mulSpaceTimeIncreaser = 5
divSpaceTimeIncreaser = 5

-- the types used for tracking data about operators
data TokenType =
  T_Int
  | T_Bit
  | T_Array TokenType Int -- Int here is the length
  deriving (Eq, Show)

len :: TokenType -> Int
len T_Int = 8
len T_Bit = 1
len (T_Array t i) = i * len t

class Addable a where
  (|+|) :: a -> a -> a

-- first int tracks number of ops, second int tracks wiring consumed
data OpsWireArea = OWA {opsArea :: Int, wireArea :: Int} deriving (Eq, Show)

instance Addable OpsWireArea where
  -- Note: need more realistic area approximation
  (|+|) (OWA o0 w0) (OWA o1 w1) = OWA (o0 + o1) (w0 + w1)

-- seq time tracks number of clock cycles, comb time tracks max combinational
-- path time 
data SeqCombTime = SCTime {seqTime :: Int, combTime :: Int} deriving (Eq, Show)

instance Addable SeqCombTime where
  -- if either is just a combinational element, combinational time increases
  -- and num cycles is constant
  (|+|) (SCTime s0 c0) (SCTime s1 c1) | s0 == 0 || s1 == 0 =
    SCTime (max s0 s1) (c0 + c1)
  -- if both are sequential, assume registers at end of each op
  (|+|) (SCTime s0 c0) (SCTime s1 c1) =
    SCTime (s0 + s1) (max c0 c1)

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
  space Mem_Read t = OWA (len t) (len t)
  space Mem_Write t = space (Mem_Read t)
  -- assuming reads are 
  time _ = SCTime 0 RWtime
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
