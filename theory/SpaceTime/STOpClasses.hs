{-# LANGUAGE GADTS #-}
module SpaceTime.STOpClasses where
import SpaceTime.STTypes
import SpaceTime.STMetrics

-- the typeclasses that all the elements of the IR must implement
class SpaceTime a where
  -- for wire space, only counting input wires, not outputs. This avoids
  -- double counting
  space :: a -> OpsWireArea
  -- this is the time to process one or more firings
  time :: a -> SeqCombTime
  -- local vs global utilization? - I think this handles both well enough 
  -- for now, can revisit later
  util :: a -> Float
  inPortsType :: a -> [PortType]
  outPortsType :: a -> [PortType]
  numFirings :: a -> Int

-- is there a better utilization than weighted by operator area
utilWeightedByArea :: (SpaceTime a) => [a] -> Float
utilWeightedByArea ops = unnormalizedUtil / (fromIntegral $ length ops)
    where unnormalizedUtil = foldl (+) 0 $
            map (\op -> (fromIntegral $ opsArea $ space op) * (util op)) ops

data Compose a =
  ComposeContainer a :: 
  | ComposePar [Compose a]
  | ComposeSeq [Compose a]
  deriving (Eq, Show)

instance (SpaceTime a) => SpaceTime (Compose a) where
  space (ComposeContainer op) = space op
  space (ComposePar ops) = foldl (|+|) addId $ map space ops
  space (ComposeSeq ops) = foldl (|+|) addId $ map space ops

  time (ComposeContainer op) = time op
  -- this depends the constructors verifying that only composing in parallel
  -- things that take same amount of time
  -- should this be equal clocks and max combinational? Should this depend on stream length?
  -- it should be fine to just check clocks and return max combuinational. The stream lenghts don't matter as long as total time the same.
  -- can have different stream lengths as long as streams take same amount of time to finish
  -- space time helpers for compose that are used for all implementations
  time (ComposePar ops) = SCTime (seqTime $ time $ head ops) maxCombTime
    where maxCombTime = maximum $ map (combTime . time) ops
  time (ComposeSeq ops) = foldl (|+|) addId $ map time ops

  util (ComposeContainer op) = util op
  util (ComposePar ops) = utilWeightedByArea ops
  util (ComposeSeq ops) = utilWeightedByArea ops

  inPortsType (ComposeContainer op) = inPortsType op
  inPortsType (ComposePar ops) = foldl (++) [] $ portsScaledByFiringPerOp inPortsType ops
  inPortsType (ComposeSeq ops) = scalePortsStreamLens (numFirings opHd) (inPortsType opHd)
    where opHd = head ops

  outPortsType (ComposeContainer op) = outPortsType op
  outPortsType (ComposePar ops) = foldl (++) [] $ portsScaledByFiringPerOp outPortsType ops
  outPortsType (ComposeSeq ops) = scalePortsStreamLens (numFirings opLst) (outPortsType opLst)
    where opLst = last ops

  numFirings _ = 1

-- This is for making ComposeSeq
(|.|) :: (SpaceTime a) => Maybe (Compose a) -> Maybe (Compose a) -> Maybe (Compose a)
(|.|) (Just op0@(ComposeSeq ops0)) (Just op1@(ComposeSeq ops1)) | canComposeSeq op1 op0 =
Just $ ComposeSeq $ ops1 ++ ops0
(|.|) (Just op0@(ComposeSeq ops0)) (Just op1) | canComposeSeq op1 op0 =
Just $ ComposeSeq $ [op1] ++ ops0
(|.|) (Just op0) (Just op1@(ComposeSeq ops1)) | canComposeSeq op1 op0 =
Just $ ComposeSeq $ ops1 ++ [op0]
(|.|) (Just op0) (Just op1) | canComposeSeq op1 op0 =
Just $ ComposeSeq $ [op1] ++ [op0]
(|.|) _ _ = Nothing

-- This is for making ComposePar
(|&|) :: (SpaceTime a) => Maybe (Compose a) -> Maybe (Compose a) -> Maybe (Compose a)
(|&|) (Just op0@(ComposePar ops0)) (Just op1@(ComposePar ops1)) | canComposePar op1 op0 =
  Just $ ComposePar $ ops0 ++ ops1
(|&|) (Just op0@(ComposePar ops0)) (Just op1) | canComposePar op1 op0 =
  Just $ ComposePar $ [op1] ++ ops0
(|&|) (Just op0) (Just op1@(ComposePar ops1)) | canComposePar op1 op0 =
  Just $ ComposePar $ ops1 ++ [op0]
(|&|) (Just op0) (Just op1) | canComposePar op1 op0 =
  Just $ ComposePar $ [op1] ++ [op0]
(|&|) _ _ = Nothing

-- This is in same spirit as Monad's >>=, kinda abusing notation
-- It's |.| in reverse so that can create pipelines in right order
(|>>=|) :: Composable a => Maybe a -> Maybe a -> Maybe a
(|>>=|) op0 op1 = op1 |.| op0

canComposeSeq :: (SpaceTime a) => a -> a -> Bool

-- only join two sequential nodes if token types match, ports do same number of tokens
-- over all firings and streams per firing, and if same number of clock cycles
canComposeSeq op0 op1 | (seqTime . time) op0 > 0 && (seqTime . time) op1 > 0 =
  -- this checks both token types and numTokens over all firing/stream combos
  portsScaledByFiringPerOp outPortsType [op0] == portsScaledByFiringPerOp inPortsType [op1] &&
  (seqTime . time) op0 == (seqTime . time) op1

-- can join a combinational node with another node if they do the same amount
-- every clock cycle
canComposeSeq op0 op1 = ((map pTType) . outPortsType) op0 ==
  ((map pTType) . inPortsType) op1

canComposePar :: (SpaceTime a) => a -> a -> Bool
-- only join two nodes in parallel if same number of clocks
canComposePar op0 op1 = (seqTime . time) op0 == (seqTime . time) op1

-- Since can compose things with different numbers of firings as long as total 
-- numbers of tokens and time, need composes to each have 1 firing and put
-- children ops' firings in its stream lengths
portsScaledByFiringPerOp :: (SpaceTime a) => (a -> [PortType]) -> [a] -> [[PortType]]
portsScaledByFiringPerOp portGetter ops = map scalePerOp ops
  where scalePerOp op = scalePortsStreamLens (numFirings op) $ portGetter op
