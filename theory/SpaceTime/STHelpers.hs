module SpaceTime.STHelpers where
-- constant use for scaling operations in space and time
rwTime = 1
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

class MergeOrScale a where
  (|+|) :: a -> a -> a
  (|*) :: a -> Int -> a
  (|/) :: a -> Int -> a


-- first int tracks number of ops, second int tracks wiring consumed
data OpsWireArea = OWA {opsArea :: Int, wireArea :: Int} deriving (Eq, Show)

-- do division and log on ints and return result rounded up to ceiling
ceilDiv :: Int -> Int -> Int
ceilDiv dividend divisor = fromIntegral $ toInteger $ ceiling
  ((fromIntegral dividend) / (fromIntegral divisor))

ceilLog :: Int -> Int
ceilLog a = fromIntegral $ toInteger $ ceiling $ logBase 2 $ fromIntegral a

instance MergeOrScale OpsWireArea where
  -- Note: need more realistic area approximation
  (|+|) (OWA o0 w0) (OWA o1 w1) = OWA (o0 + o1) (w0 + w1)
  (|*) (OWA o w) i = OWA (o * i) (w * i)
  -- taking ceiling to be conservative
  (|/) (OWA o w) i = OWA (o `ceilDiv` i) (w `ceilDiv` i)

-- the space of adding a counter that counts to I, assuming efficient counter
-- that uses log bits for area and time
counterSpace :: Int -> OpsWireArea
counterSpace countTo = OWA numBits numBits
  where numBits = ceilLog countTo

registerSpace :: TokenType -> OpsWireArea
registerSpace op = OWA (len op) (len op)

-- seq time tracks number of clock cycles, comb time tracks max combinational
-- path time 
data SeqCombTime = SCTime {seqTime :: Int, combTime :: Int} deriving (Eq, Show)

instance MergeOrScale SeqCombTime where
  -- if either is just a combinational element, combinational time increases
  -- and num cycles is constant
  (|+|) (SCTime s0 c0) (SCTime s1 c1) | s0 == 0 || s1 == 0 =
    SCTime (max s0 s1) (c0 + c1)
  -- if both are sequential, assume registers at end of each op
  (|+|) (SCTime s0 c0) (SCTime s1 c1) = SCTime (s0 + s1) (max c0 c1)
  -- when scaling up/down combinational, combinational time gets longer
  -- otherwise sequential time gets longer
  (|*) (SCTime s c) i | s == 0 = SCTime 0 (c*i)
  (|/) (SCTime s c) i | s == 0 = SCTime 0 (c*i)

registerTime :: SeqCombTime
registerTime = SCTime {1, 1}
