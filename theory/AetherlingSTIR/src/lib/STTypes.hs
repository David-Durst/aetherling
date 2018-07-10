module STTypes where

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

-- type used to pass values to the simulator.
-- There is a V_Type value type corresponding to each T_Type TokenType.
data ValueType =
  V_Unit 
  | V_Int Int
  | V_Bit Bool
  | V_Array [ValueType]
  deriving (Eq, Show)

instance HasLen ValueType where
  len V_Unit = 0
  len (V_Int _) = 8
  len (V_Bit _) = 1
  len (V_Array list) = length list

-- Function for checking that a ValueType instance matches the given
-- TokenType instance. Also checks that arrays are the same length.
-- V_Unit matches all types.
vtTypesMatch :: ValueType -> TokenType -> Bool
vtTypesMatch (V_Unit) t = True
vtTypesMatch (V_Int _) T_Int = True
vtTypesMatch (V_Bit _) T_Bit = True
vtTypesMatch (V_Array []) (T_Array i t) = i == 0
vtTypesMatch (V_Array (v:vs)) (T_Array i t) =
    vtTypesMatch v t && vtTypesMatch (V_Array vs) (T_Array (i-1) t)
vtTypesMatch _ _ = False

-- Reverse argument order of above.
tvTypesMatch :: TokenType -> ValueType -> Bool
tvTypesMatch t v = vtTypesMatch v t

-- Head and tail functions for V_Array type (no-op for V_Unit).
valueTypeHead :: ValueType -> ValueType
valueTypeHead V_Unit = V_Unit
valueTypeHead (V_Array arr) = head arr
valueTypeHead _ = error "valueTypeHead of non-array non-unit ValueType"

valueTypeTail :: ValueType -> ValueType
valueTypeTail V_Unit = V_Unit
valueTypeTail (V_Array arr) = V_Array (tail arr)
valueTypeTail _ = error "valueTypeTail of non-array non-unit ValueType"

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

baseWithNoWarmupSequenceLen = SWLen 1 0

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

renamePorts :: String -> [PortType] -> [PortType]
renamePorts templateName ports = snd $ foldl renameAndIncrementCounter (0, []) ports
  where renameAndIncrementCounter (curCounter, processedPorts) (T_Port _ sLen tType pct) =
          (curCounter + 1, processedPorts ++ [T_Port (templateName ++ show curCounter) sLen tType pct])
