module SpaceTime.STTypes where

class HasLen a where
  len :: a -> Int

-- the types used for tracking data about operators
data TokenType =
  T_Unit
  | T_Int
  | T_Bit
  -- Int here is the length
  | T_Array Int TokenType
  deriving (Eq, Show)

instance HasLen TokenType where
  len T_Unit = 0
  len T_Int = 8
  len T_Bit = 1
  len (T_Array i t) = i * len t

-- the constants for describing a number of clocks or elements that is 
-- steadyStateMultiplier * n - warmupSub
data SteadyStateAndWarmupLen = SWLen {steadyStateMultiplier :: Int, warmupSub :: Int}
  deriving (Eq)

instance Show SteadyStateAndWarmupLen where
  show (SWLen ssMult wSub) | wSub > 0 = (show ssMult) ++ "n - " ++ (show wSub)
  show (SWLen ssMult wSub) | wSub == 0 = (show ssMult) ++ "n"
  show (SWLen ssMult wSub) = (show ssMult) ++ "n + " ++ (show (-1 * wSub))

addToWarmup wIncr (SWLen ssMult wSub) = SWLen ssMult (wSub + wIncr)
multToSteadyState ssScaler (SWLen ssCur wSub) = SWLen (ssCur * ssScaler) wSub
makeSWLenConcrete nLen (SWLen ssMult wSub) = ssMult * nLen - wSub

baseWithNoWarmupStreamLen = SWLen 1 0
-- implicitly not banning multiple ports with same name here
-- names are only helpful reminders, can have duplicates with non-renamed ports
-- pCTime tracks the combinonal time from the module through this port
data PortType = T_Port {pName :: [Char], pSeqLen :: SteadyStateAndWarmupLen, 
  pTType :: TokenType, pCTime :: Int} deriving (Show)

instance Eq PortType where
  -- ignore names for equality, just check that all same
  (==) (T_Port _ len0 tType0 pct0) (T_Port _ len1 tType1 pct1) = 
    len0 == len1 && tType0 == tType1 && pct0 == pct1
  (/=) pt0 pt1 = not $ pt0 == pt1

duplicatePorts :: Int -> [PortType] -> [PortType]
duplicatePorts n ports = map wrapInArray ports
  where wrapInArray (T_Port name sLen t pct) = T_Port name sLen (T_Array n t) pct

