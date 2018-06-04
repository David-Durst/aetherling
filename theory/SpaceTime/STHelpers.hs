module SpaceTime.STHelpers where
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
  (|+|) :: a -> a -> a
  (|*) :: a -> Int -> a
  (|/) :: a -> Int -> a

class HasLen a where
  len :: a -> Int

-- the types used for tracking data about operators
data TokenType =
  T_Unit
  | T_Int
  | T_Bit
  deriving (Eq, Show)

instance HasLen TokenType where
  len T_Unit = 0
  len T_Int = 8
  len T_Bit = 1

-- Int here is the length
data TokensType = T_Array TokenType Int deriving (Eq, Show)

instance HasLen TokensType where
  len (T_Array t i) = i * len t

-- implicitly not banning multiple ports with same name here
-- names are only helpful reminders, can have duplicates with non-renamed ports
data PortsType = T_Ports [([Char], TokensType)] deriving (Show)

instance Eq PortsType where
  -- ignore names for equality, just check that all same
  (==) (T_Ports ports0) (T_Ports ports1) = 
    (length ports0 == length ports1) && 
    (foldl (&&) True $ zipWith (\port0 port1 -> (port0 == port1)) ports0 ports1)
  (/=) pt0 pt1 = not $ pt0 == pt1

instance HasLen PortsType where
  len (T_Ports ports) = foldl (+) 0 $ map (len . snd) ports

instance MergeOrScale PortsType where
  (|+|) (T_Ports ports0) (T_Ports ports1) = T_Ports $ ports0 ++ ports1
  (|*) (T_Ports ports) i = T_Ports $ map mulArrayLen ports
    where mulArrayLen (portName, T_Array tType arrLen) =
            (portName, T_Array tType (arrLen * i))
  (|/) (T_Ports ports) i = T_Ports $ map divArrayLen ports
    where divArrayLen (portName, T_Array tType arrLen) =
            (portName, T_Array tType (arrLen `ceilDiv` i))

portsFromTokens :: [([Char], Int, TokenType)] -> PortsType
portsFromTokens ts = T_Ports $ map makeTokens ts
  where makeTokens (tName, tNum, tType) = (tName, T_Array tType tNum)

-- first int tracks number of ops, second int tracks wiring consumed
data OpsWireArea = OWA {opsArea :: Int, wireArea :: Int} deriving (Eq, Show)

owAreaZero = OWA 0 0

-- the space of adding a counter that counts to I, assuming efficient counter
-- that uses log bits for area and time
counterSpace :: Int -> OpsWireArea
counterSpace countTo = OWA numBits numBits
  where numBits = ceilLog countTo

registerSpace :: PortsType -> OpsWireArea
registerSpace op = OWA (len op) (len op)

instance MergeOrScale OpsWireArea where
  -- Note: need more realistic area approximation
  (|+|) (OWA o0 w0) (OWA o1 w1) = OWA (o0 + o1) (w0 + w1)
  (|*) (OWA o w) i = OWA (o * i) (w * i)
  -- taking ceiling to be conservative
  (|/) (OWA o w) i = OWA (o `ceilDiv` i) (w `ceilDiv` i)

-- seq time tracks number of clock cycles, comb time tracks max combinational
-- path time 
data SeqCombTime = SCTime {seqTime :: Int, combTime :: Int} deriving (Eq, Show)

scTimeZero = SCTime 0 0
registerTime = SCTime 1 1

isCombNode :: SeqCombTime -> Bool
isCombNode (SCTime s _) = s == 0

instance MergeOrScale SeqCombTime where
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
replicateTimeOverStream :: SeqCombTime -> Int -> SeqCombTime
replicateTimeOverStream t@(SCTime s _) i | s == 0 = t |+| (registerTime |* i)
replicateTimeOverStream t@(SCTime s _) i = t |* i

-- This tracks both the input and output streams to an op
-- numFiring is 0 means streams are part of an op that aren't wrapped in an
-- iterate
data IOStreamLens = IOSLens { inOneFiringLen :: Int, outOneFiringLen :: Int,
  numFirings :: Int } deriving (Eq, Show)

