module SpaceTime.STMetrics where
import SpaceTime.STTypes

-- helpful functions and constants
-- constant use for scaling operations in space and time
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

-- first int tracks number of ops, second int tracks wiring consumed
data OpsWireArea = OWA {opsArea :: Int, wireArea :: Int} deriving (Eq, Show)

-- the space of adding a counter that counts to I, assuming efficient counter
-- that uses log bits for area and time
counterSpace :: Int -> OpsWireArea
counterSpace countTo = OWA numBits numBits
  where numBits = ceilLog countTo

-- The amount of space necessary to store tokens
registerSpace :: [TokenType] -> OpsWireArea
registerSpace ts = OWA portsLen portsLen
  where portsLen = foldl (+) 0 $ map len ts

instance MergeOrScale OpsWireArea where
  addId = OWA 0 0
  -- Note: need more realistic area approximation
  (|+|) (OWA o0 w0) (OWA o1 w1) = OWA (o0 + o1) (w0 + w1)
  (|*) (OWA o w) i = OWA (o * i) (w * i)

data SteadyStateAndWarmupRatio = SWRatio {swNumerator :: SteadyStateAndWarmupLen, 
  swDenominator :: SteadyStateAndWarmupLen}
  deriving (Eq)

instance Show SteadyStateAndWarmupRatio where
  show (SWRatio num denom) | num == denom = "1"
  show (SWRatio (SWLen numMult numWarmup) (SWLen denomMult denomWarmup)) | numWarmup == 0 &&
    denomWarmup == 0 = show (SWLen (numMult `ceilDiv` denomMult) 0)
  show (SWRatio num denom) = "(" ++ show num ++ ") / (" ++ show denom ++ ")"

data PortThroughput = PortThroughput {throughputType :: TokenType, throughputClocks :: SteadyStateAndWarmupRatio}
  deriving (Show, Eq)
