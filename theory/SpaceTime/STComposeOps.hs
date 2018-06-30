module SpaceTime.STComposeOps where
import SpaceTime.STTypes
import SpaceTime.STAST

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

-- This is in same spirit as Monad's >>=, kinda abusing notation
-- It's |.| in reverse so that can create pipelines in right order
(|>>=|) :: Op -> Op -> Op
(|>>=|) op0 op1 = op1 |.| op0

canComposeSeq :: Op -> Op -> Bool

-- only join two sequential nodes if same numbers of ports, toke types match,
-- and steady state throughputs match
canComposeSeq op0 op1 | (length . outPorts) op0 == (length . inPorts) op1 =
  reduce (&&) True $ map portPairMatches (zip (outPorts op0) (inPorts op1))
  where
    portPairMatches (T_Port _ sLen0 tType0 _) (T_Port _ SLen1 tType1 _) = (sLen0 ==
      sLen1) && (tType0 == tType1)
canComposeSeq _ _ = False

(|&|) :: Op -> Op -> Op
(|&|) (ComposePar ops0) (ComposePar ops1) = ComposePar $ ops0 ++ ops1
(|&|) (ComposePar ops0) op1 = ComposePar $ ops0 ++ [op1]
(|&|) op0 (ComposePar ops1) = ComposePar $ [op0] ++ ops1
(|&|) op0 op1 = ComposePar $ [op1] ++ [op0]
