module SpaceTime.STComposeOps where
import SpaceTime.STTypes
import SpaceTime.STMetrics
import SpaceTime.STAST
import SpaceTime.STAnalysis

-- This is for making ComposeSeq
(|.|) :: Op -> Op -> Op
-- if failed in earlier step, keep propagating failures
(|.|) cf@(ComposeFailure _ _) op1 = ComposeFailure PriorFailure (cf, op1)
(|.|) op0 cf@(ComposeFailure _ _) = ComposeFailure PriorFailure (op0, cf)
-- when checking if can compose, need to match up individual elements, not whole list
-- ex. If each component is operating at one token per 10 clocks, sequence of 4
-- parts will take 40 clocks, but should be able to add another component 
-- operating at one token per 10 clocks to get a sequence of 5 parts at 50 clocks
(|.|) (op0@(ComposeSeq ops0)) (op1@(ComposeSeq ops1)) 
  | canComposeSeq op1 op0 == ComposeSuccess = ComposeSeq $ ops1 ++ ops0
(|.|) (op0@(ComposeSeq ops0)) (op1) | canComposeSeq op1 op0 == ComposeSuccess =
  ComposeSeq $ [op1] ++ ops0
(|.|) (op0) (op1@(ComposeSeq ops1)) | canComposeSeq op1 op0 == ComposeSuccess =
  ComposeSeq $ ops1 ++ [op0]
(|.|) (op0) (op1) | canComposeSeq op1 op0 == ComposeSuccess =
  ComposeSeq $ [op1] ++ [op0]
(|.|) op0 op1 = ComposeFailure SeqPortMismatch (op0, op1)

-- This is for making ComposePar
(|&|) :: Maybe Op -> Maybe Op -> Maybe Op
-- if failed in earlier step, keep propagating failures
(|&|) cf@(ComposeFailure _ _) op1 = ComposeFailure PriorFailure (cf, op1)
(|&|) op0 cf@(ComposeFailure _ _) = ComposeFailure PriorFailure (op0, cf)
(|&|) (op0@(ComposePar ops0)) (op1@(ComposePar ops1)) | canComposePar op1 op0 == ComposeSuccess =
  ComposePar $ ops0 ++ ops1
(|&|) (op0@(ComposePar ops0)) (op1) | canComposePar op1 op0 == ComposeSuccess =
  ComposePar $ [op1] ++ ops0
(|&|) (op0) (op1@(ComposePar ops1)) | canComposePar op1 op0 == ComposeSuccess =
  ComposePar $ ops1 ++ [op0]
(|&|) (op0) (op1) | canComposePar op1 op0 == ComposeSuccess =
  ComposePar $ [op1] ++ [op0]
(|&|) op0 op1 = canComposePar op0 op1

-- This is in same spirit as Monad's >>=, kinda abusing notation
-- It's |.| in reverse so that can create pipelines in right order
(|>>=|) :: Op -> Op -> Op
(|>>=|) op0 op1 = op1 |.| op0

canComposeSeq :: Op -> Op -> Bool

-- only join two sequential nodes if token types match and throughputs are equal
canComposeSeq op0 op1 | (seqTime . time) op0 > 0 && (seqTime . time) op1 > 0 =
  -- this checks both token types and numTokens over all firing/stream combos
  outPorts op0 == inPorts op1 && 
  (numClocks . pipelineTime) op0 == (numClocks . pipelineTime) op1

-- can join a combinational node with another node if they do the same amount
-- every clock cycle
canComposeSeq op0 op1 = ((map pTType) . outPorts) op0 ==
  ((map pTType) . inPorts) op1

canComposePar :: Op -> Op -> Bool
-- only join two nodes in parallel if same number of clocks
-- don't think need equal lengths, just producing same amount every clock,
-- right?Do this however so can compute time more easily and easier to connect
-- composePar to other things. Users just need to underutilize one pipeline
-- which is explicit version of what allowing two different timed things to
-- run in parallel is anyway
canComposePar op0 op1 = (seqTime . time) op0 == (seqTime . time) op1 && 
  (numClocks . pipelineTime) op0 == (numClocks . pipelineTime) op1

-- Since can compose things with different numbers of firings as long as total 
-- numbers of tokens and time, need composes to each have 1 firing and put
-- children ops' firings in its stream lengths
-- portsScaledByFiringPerOp ::  => (a -> [PortType]) -> [a] -> [[PortType]]
-- portsScaledByFiringPerOp portGetter ops = map scalePerOp ops
--  where scalePerOp op = scalePortsStreamLens (numFirings op) $ portGetter op
