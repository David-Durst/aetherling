module SpaceTime.STMetrics where
import SpaceTime.STTypes

-- helpful functions and constants
-- constant use for scaling operations in space and time
rwTime :: Int
rwTime = 1
mulSpaceTimeIncreaser :: Int
mulSpaceTimeIncreaser = 5
divSpaceTimeIncreaser :: Int
divSpaceTimeIncreaser = 5

-- do division and log on ints and return result rounded up to ceiling
ceilDiv :: Int -> Int -> Int
ceilDiv dividend divisor = fromIntegral $ toInteger $ ceiling
  ((fromIntegral dividend) / (fromIntegral divisor))

ceilLog :: Int -> Int
ceilLog a = fromIntegral $ toInteger $ ceiling $ logBase 2 $ fromIntegral a

class MergeOrScale a where
  addId :: a
  (|+|) :: a -> a -> a
  (|*) :: a -> Int -> a
  (|/) :: a -> Int -> a

-- first int tracks number of ops, second int tracks wiring consumed
data OpsWireArea = OWA {opsArea :: Int, wireArea :: Int} deriving (Eq, Show)

-- the space of adding a counter that counts to I, assuming efficient counter
-- that uses log bits for area and time
counterSpace :: Int -> OpsWireArea
counterSpace countTo = OWA numBits numBits
  where numBits = ceilLog countTo

-- The amount of space necessary to store tokens
registerSpace :: [TokensType] -> OpsWireArea
registerSpace ts = OWA portsLen portsLen
  where portsLen = foldl (+) 0 $ map len ts

instance MergeOrScale OpsWireArea where
  addId = OWA 0 0
  -- Note: need more realistic area approximation
  (|+|) (OWA o0 w0) (OWA o1 w1) = OWA (o0 + o1) (w0 + w1)
  (|*) (OWA o w) i = OWA (o * i) (w * i)
  -- taking ceiling to be conservative
  (|/) (OWA o w) i = OWA (o `ceilDiv` i) (w `ceilDiv` i)

-- seq time tracks number of clock cycles, comb time tracks max combinational
-- path time 
data SeqCombTime = SCTime {seqTime :: Int, combTime :: Int} deriving (Eq, Show)

registerTime = SCTime 1 1

isCombNode :: SeqCombTime -> Bool
isCombNode (SCTime s _) = s == 0

instance MergeOrScale SeqCombTime where
  addId = SCTime 0 0
  -- if either is just a combinational element, combinational time increases
  -- and num cycles is constant
  (|+|) (SCTime s0 c0) (SCTime s1 c1) | s0 == 0 || s1 == 0 =
    SCTime (max s0 s1) (c0 + c1)
  -- if both are sequential, assume registers at end of each op
  (|+|) (SCTime s0 c0) (SCTime s1 c1) = SCTime (s0 + s1) (max c0 c1)
  -- when scaling up/down combinational, combinational time gets longer
  -- otherwise sequential time gets longer
  (|*) (SCTime s c) i | s == 0 = SCTime 0 (c * i)
  (|*) (SCTime s c) i = SCTime (s * i) c
  (|/) (SCTime s c) i | s == 0 = SCTime 0 (c `ceilDiv` i)
  (|/) (SCTime s c) i = SCTime (s `ceilDiv` i) c

-- given a SeqCombTime and a stream length, return its time assuming registers
-- are at the end of each element of stream
replicateTimeOverStream :: Int -> SeqCombTime -> SeqCombTime
replicateTimeOverStream i t@(SCTime s _) | s == 0 = t |+| (registerTime |* i)
replicateTimeOverStream i t@(SCTime s _) = t |* i
