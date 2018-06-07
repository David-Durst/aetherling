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

-- implicitly not banning multiple ports with same name here
-- names are only helpful reminders, can have duplicates with non-renamed ports
data PortType = T_Port {pName :: [Char], pStreamLen :: Int, pTType :: TokenType} 
  deriving (Show)

instance Eq PortType where
  -- ignore names for equality, just check that all same
  (==) (T_Port _ len0 tType0) (T_Port _ len1 tType1) = 
    len0 == len1 && tType0 == tType1
  (/=) pt0 pt1 = not $ pt0 == pt1

duplicatePorts :: Int -> [PortType] -> [PortType]
duplicatePorts n ports = map wrapInArray ports
  where wrapInArray (T_Port name sLen t) = T_Port name sLen (T_Array n t)

scalePortsStreamLens :: Int -> [PortType] -> [PortType]
scalePortsStreamLens n ports = map mulPortStreamLen ports 
  where mulPortStreamLen (T_Port pName pSLen tType) = T_Port pName (n*pSLen) tType
