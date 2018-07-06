module STMetrics where
import STTypes
import Data.Ratio

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

-- A space for a linear shift register
-- only need input and output wires (and not counting output wires by convention)
-- as all buffers except first connect to each output of prior one
rowbufferSpace :: Int -> TokenType -> OpsWireArea
rowbufferSpace n ts = OWA (len ts * n) (len ts)

instance MergeOrScale OpsWireArea where
  addId = OWA 0 0
  -- Note: need more realistic area approximation
  (|+|) (OWA o0 w0) (OWA o1 w1) = OWA (o0 + o1) (w0 + w1)
  (|*) (OWA o w) i = OWA (o * i) (w * i)

data SteadyStateAndWarmupRatio = SWRatio {swNumerator :: SteadyStateAndWarmupLen, 
  swDenominator :: SteadyStateAndWarmupLen}

instance Show SteadyStateAndWarmupRatio where
  show (SWRatio num denom) | num == denom = "1"
  show (SWRatio (SWLen numMult numWarmup) (SWLen denomMult denomWarmup)) | numWarmup == 0 &&
    denomWarmup == 0 = "(" ++ show (numerator simplified) ++ "/" ++ show (denominator simplified) ++ ")n"
    where simplified = numMult % denomMult
  show (SWRatio num denom) = "(" ++ show num ++ ") / (" ++ show denom ++ ")"

instance Eq SteadyStateAndWarmupRatio where
  -- this is (an+b)/(cn+d) == (en+f)/(gn+h)
  -- which is a*gn^2 + a*hn + b*gn + b*h == c*en^2 + c*fn + d*en + d*f
  (==) (SWRatio (SWLen a b) (SWLen c d))
    (SWRatio (SWLen e f) (SWLen g h))
    = (a*g == c*e) && (a*h + b*g == c*f + d*e) && (b*h == d*f)
  (/=) ratio0 ratio1 = not (ratio0 == ratio1) 

data PortThroughput = PortThroughput {throughputType :: TokenType, 
  throughputTypePerClock :: SteadyStateAndWarmupRatio} deriving (Show, Eq)
