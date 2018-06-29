module SpaceTime.STOps where
import SpaceTime.STTypes
import SpaceTime.STMetrics
import Data.Typeable
import Data.Bool
import Data.Ratio

-- These are leaf nodes that can be used in a higher order operator
data Op =
  -- LEAF OPS
  Add TokenType
  | Sub TokenType
  | Mul TokenType
  | Div TokenType
  | MemRead TokenType
  | MemWrite TokenType
  -- first Int is pixels per clock, second is window width, third int is 
  | LineBuffer {pxPerClock :: Int, windowWidth :: Int, lbInT :: TokenType}
  -- Array is constant produced, int is sequence length
  | Constant_Int {intConstProduced :: [Int]}
  -- Array is constant produced, int is sequence length
  | Constant_Bit {bitConstProduced :: [Bool]}
  -- first pair is input subsequence length and tokens per element, second is output
  | SequenceArrayController (Int, TokenType) (Int, TokenType)

  -- HIGHER ORDER OPS
  | MapOp {mapParallelism :: Int, mappedOp :: Op}
  | ReduceOp {reduceParallelism :: Int, reduceNumCombined :: Int, reducedOp :: Op}

  -- run underOp at CPS = utilDenominator * old CPS
  | Underutil {utilDenominator :: Int, underutilizedOp :: Op}
  -- this inceases latency
  | RegDelay {delayClocks :: Int, delayedOp :: Op}

  -- COMPOSE OPS
  | ComposePar [Op]
  | ComposeSeq [Op]
  | ComposeFailure ComposeResult (Op, Op) 
  deriving (Eq, Show)

-- for wire space, only counting input wires, not outputs. This avoids
-- double counting
space :: Op -> OpsWireArea
space (Add t) = OWA (len t) (2 * len t)
space (Sub t) = space (Add t)
space (Mul t) = OWA (mulSpaceTimeIncreaser * len t) wireArea
  where OWA _ wireArea = space (Add t)
space (Div t) = OWA (divSpaceTimeIncreaser * len t) wireArea
  where OWA _ wireArea = space (Add t)
space (MemRead t) = OWA 0 (len t)
space (MemWrite t) = OWA (len t) (len t)
-- need registers for storing intermediate values
-- registers account for wiring as some registers receive input wires,
-- others get wires from other registers
-- add |+| counterSpace (p `ceilDiv` w) when accounting for warmup counter
space (LineBuffer p w t) = registerSpace [T_Array (p + w - 1) t]
space (Constant_Int consts) = OWA (len (T_Array (length consts) T_Int)) 0
space (Constant_Bit consts) = OWA (len (T_Array (length consts) T_Bit)) 0
-- just a pass through, so will get removed by CoreIR
space (SequenceArrayController (inSLen, _) (outSLen, _)) | 
  inSLen == 1 && outSLen == 1 = addId
-- may need a more accurate approximate, but most conservative is storing
-- entire input
space (SequenceArrayController (inSLen, inType) _) = registerSpace [inType] |* inSLen

-- area of parallel map is area of all the copies
space (MapOp par op) = (space op) |* par
-- area of reduce is area of reduce tree, with area for register for partial
-- results and counter for tracking iteration time if input is sequence of more
-- than what is passed in one clock
space (ReduceOp par numComb op) | par == numComb = (space op) |* (par - 1)
space rOp@(ReduceOp par numComb op) =
  reduceTreeSpace |+| (space op) |+| (registerSpace $ map pTType $ outPorts op)
  |+| (counterSpace $ numComb * denomSSMult `ceilDiv` numSSMult)
  where 
    reduceTreeSpace = space (ReduceOp par par op)
    -- need to be able to count all clocks in steady state, as that is when
    -- will be doing reset every nth
    -- thus, divide numComb by throuhgput in steady state to get clocks for
    -- numComb to be absorbed
    -- only need throughput from first port as all ports have same throuhgput
    (_, SWRatio (SWLen numSSMult _) (SWLen denomSSMult _)) = portThroughput op $ head $ inPorts op

space (Underutil denom op) = space op |+| counterSpace denom
space (RegDelay dc op) = space op |+|
  ((registerSpace $ map pTType $ outPorts op) |* dc)

space (ComposePar ops) = foldl (|+|) addId $ map space ops
space (ComposeSeq ops) = foldl (|+|) addId $ map space ops
space (ComposeFailure _ _) = 0

-- scaleCPS depending on if Op is combinational or not
scaleCPS :: Op -> Int -> SteadyStateAndWarmupLen
scaleCPS op n | isComb op = baseWithNoWarmupSequenceLen
scaleCPS op n = SWLen (ssMult * n) (wSub * n)
  where (SWLen ssMult wSub) = cps op

cps op = clocksPerSequence op
clocksPerSequence :: Op -> SteadyStateAndWarmupLen
registerCPS = baseWithNoWarmupSequenceLen

clocksPerSequence (Add t) = baseWithNoWarmupSequenceLen
clocksPerSequence (Sub t) = baseWithNoWarmupSequenceLen
clocksPerSequence (Mul t) = baseWithNoWarmupSequenceLen
clocksPerSequence (Div t) = baseWithNoWarmupSequenceLen
-- to what degree can we pipeline MemRead and MemWrite
clocksPerSequence (MemRead _) = baseWithNoWarmupSequenceLen
clocksPerSequence (MemWrite _) = baseWithNoWarmupSequenceLen
clocksPerSequence (LineBuffer _ _ _) = baseWithNoWarmupSequenceLen
clocksPerSequence (Constant_Int _) = baseWithNoWarmupSequenceLen
clocksPerSequence (Constant_Bit _) = baseWithNoWarmupSequenceLen
-- since one of the lengths must divide the other (as must be able to cleanly)
-- divide/group the input into the output, just take max as that is lcm
clocksPerSequence (SequenceArrayController (inSLen, _) (outSLen, _)) = SWLen (max inSLen outSLen) 0

clocksPerSequence (MapOp _ op) = cps op
-- always can pipeline. Just reset register every numComb/par if not fully parallel
clocksPerSequence (ReduceOp par _ op) = cps op

clocksPerSequence (Underutil denom op) = multToSteadyState denom $ clocksPerSequence op
-- since pipelined, this doesn't affect clocks per stream
clocksPerSequence (RegDelay _ op) = clocksPerSequence op

clocksPerSequence (ComposePar ops) = SWLen lcmSteadyState maxWarmup
  where 
    maxWarmup = maximum $ map (warmupSub . cps) ops
    -- 1 works as all integers for steady state >= 1
    lcmSteadyState :: Int
    lcmSteadyState = foldl lcm 1 $ map (steadyStateMultiplier . cps) ops
-- this depends on only wiring up things that have matching throughputs
clocksPerSequence (ComposeSeq ops) = SWLen lcmSteadyState sumWarmup
  where
    sumWarmup = sum $ map (warmupSub . cps) ops
    lcmSteadyState = foldl lcm 1 $ map (steadyStateMultiplier . cps) ops
clocksPerSequence (ComposeFailure _ _) = 0


registerInitialLatency = 1
initialLatency :: Op -> Int
initialLatency (Add t) = 1
initialLatency (Sub t) = 1
initialLatency (Mul t) = 1
initialLatency (Div t) = 1
initialLatency (MemRead _) = 1
initialLatency (MemWrite _) = 1
-- for each extra element in per clock, first output is 1 larger, but get 1
-- extra every clock building up to first output
initialLatency (LineBuffer p w _) = (w + p - 1) / p
initialLatency (Constant_Int _) = 1
initialLatency (Constant_Bit _) = 1
initialLatency (SequenceArrayController (inSLen, _) (outSLen, _)) = lcm inSLen outSLen

initialLatency (MapOp _ op) = initialLatency op
initialLatency (ReduceOp par numComb op) | par `mod` numComb == 0 && isComb op = 1
initialLatency (ReduceOp par numComb op) | par `mod` numComb == 0 = initialLatency op * (ceilLog par)
initialLatency (ReduceOp par numComb op) =
  -- pipelinng means only need to wait on latency of tree first time
  reduceTreeInitialLatency + (numComb `ceilDiv` par) * (initialLatency op + registerInitialLatency)
  where 
    reduceTreeInitialLatency = initialLatency (ReduceOp par par op)
    -- op adds nothing if its combinational, its CPS else
    opCPS = bool 0 (initialLatency op) (isComb op)

initialLatency (Underutil denom op) = initialLatency op
-- since pipelined, this doesn't affect clocks per stream
initialLatency (RegDelay dc op) = initialLatency op + dc

initialLatency (ComposePar ops) = maximum $ map initialLatency ops
-- initialLatency is 1 if all elemetns are combintional, sum of latencies of sequential
-- elements otherwise
initialLatency (ComposeSeq ops) = bool combinationalInitialLatency sequentialInitialLatency
  (sequentialInitialLatency > 0)
  where 
    combinationalInitialLatency = 1
    sequentialInitialLatency = foldl (+) 0 $ map initialLatency $ filter (not . isComb) ops
initialLatency (ComposeFailure _ _) = 0

-- in order to get maxCombPath for composeSeq, need to get all combinational 
-- chains with the starting and stopping sequential nodes to get all max, multiop
-- combinational paths
getMultiOpCombGroupings (ComposeSeq ops) = 
  foldl appendIfCombNewListIfSeq [] ops
  where 
    appendIfCombNewListIfSeq :: [[Op]] -> Op -> [[Op]]
    appendIfCombNewListIfSeq listOfCombLists nextOp | length listOfCombLists == 0 = 
      [[nextOp]]
    -- if this is combinational, keep the current list going by appending nothing
    -- , else stop it by starting the next one
    appendIfCombNewListIfSeq listOfCombLists nextOp = 
      init listOfCombLists ++ [last listOfCombLists ++ [nextOp]] ++
        bool [] [[nextOp]] (isComb nextOp)

-- assuming here that all ports on a combinational module have the same comb
-- path length. Not a valid assumption, but good enough to get started
-- each list is one of the child lists from getMultiOpCombPaths, meaning
-- only valid locations for sequential elements are at start and end
getCombPathLength ops = seqStartCombLen ops + seqEndCombLen ops + sumOfCombOpPaths
  where
    -- add longest of comb lengths of ports of starting and ending sequential 
    -- ops. But only do this if the first and last elements are sequential
    -- Otherwise, this will be handled by sumOfCombOpPaths
    seqStartCombLen ops | isComb $ head ops = 0
    seqStartCombLen ops = maximum $ map pCTime (outPorts $ head ops)
    seqEndCombLen ops | isComb $ head ops = 0
    seqEndCombLen ops = maximum $ map pCTime (inPorts $ last ops)
    sumOfCombOpPaths = foldl (+) 0 $ map maxCombPath ops

maxCombPath :: Op -> Int
maxCombPath (Add t) = 1
maxCombPath (Sub t) = 1
maxCombPath (Mul t) = 1
maxCombPath (Div t) = 1
maxCombPath (MemRead _) = 1
maxCombPath (MemWrite _) = 1
maxCombPath (LineBuffer _ _ _) = 1
maxCombPath (Constant_Int _) = 1
maxCombPath (Constant_Bit _) = 1
maxCombPath (SequenceArrayController (inSLen, _) (outSLen, _)) = 1

maxCombPath (MapOp _ op) = maxCombPath op
maxCombPath (ReduceOp par _ op) | isComb op = maxCombPath op * ceilLog par
-- since connecting each op to a copy, and all are duplicates, 
-- maxCombPath is either internal to each op, or from combining two of them
maxCombPath (ReduceOp par numComb op) = max (maxCombPath op) maxCombPathFromOutputToInput
  where
    -- since same output goes to both inputs, just take max of input comb path 
    -- plus output path as that is max path
    -- assuming two inputs and one output to op
    maxCombPathFromOutputToInput = maximum (map pCTime $ inPorts op) + (pCTime $ head $ outPorts op)

maxCombPath (Underutil denom op) = maxCombPath op
-- since pipelined, this doesn't affect clocks per stream
maxCombPath (RegDelay _ op) = maxCombPath op

maxCombPath (ComposePar ops) = maximum $ map maxCombPath ops
maxCombPath compSeq@(ComposeSeq ops) = max maxSingleOpPath maxMultiOpPath
  where
    -- maxSingleOpPath gets the maximum internal combinational path of all elements
    maxSingleOpPath = maximum $ map maxCombPath ops
    maxMultiOpPath = maximum $ map getCombPathLength $ getMultiOpCombGroupings compSeq

maxCombPath (ComposeFailure _ _) = 0


util :: Op -> Float
util (Add t) = 1
util (Sub t) = 1
util (Mul t) = 1
util (Div t) = 1
util (MemRead _) = 1
util (MemWrite _) = 1
util (LineBuffer _ _ _) = 1
util (Constant_Int _) = 1
util (Constant_Bit _) = 1
util (SequenceArrayController (inSLen, _) (outSLen, _)) = 1

util (MapOp _ op) = util op
util (ReduceOp _ _ op) = util op

util (Underutil denom op) = util op / fromIntegral denom
-- since pipelined, this doesn't affect clocks per stream
util (RegDelay _ op) = util op

util (ComposePar ops) = utilWeightedByArea ops
util (ComposeSeq ops) = utilWeightedByArea ops
util (ComposeFailure _ _) = 0
-- is there a better utilization than weighted by operator area
utilWeightedByArea :: [Op] -> Float
utilWeightedByArea ops = unnormalizedUtil / totalArea
    where 
      unnormalizedUtil = foldl (+) 0 $
        map (\op -> (fromIntegral $ opsArea $ space op) * (util op)) ops
      totalArea = foldl (+) 0 $ map (fromIntegral . opsArea . space) ops


twoInSimplePorts t = [T_Port "I0" 1 t 2, T_Port "I1" 1 t 2]
inPorts :: Op -> [PortType]
inPorts (Add t) = twoInSimplePorts t
inPorts (Sub t) = twoInSimplePorts t
inPorts (Mul t) = twoInSimplePorts t
inPorts (Div t) = twoInSimplePorts t
inPorts (MemRead _) = []
inPorts (MemWrite t) = [T_Port "I" 1 t 1]
-- 2 as it goes straight through LB
inPorts (LineBuffer p _ t) = [T_Port "I" 1 (T_Array p t) 2]
inPorts (Constant_Int _) = []
inPorts (Constant_Bit _) = []
inPorts (SequenceArrayController (inSLen, inType) _) = [T_Port "I" (SWLen inSLen 0) inType 2]

inPorts (MapOp par op) = duplicatePorts par (inPorts op)
-- can just take head as can only reduce binary operators
inPorts (ReduceOp par numComb op) = duplicatePorts par [head $ inPorts op]

inPorts (RegDelay _ op) = inPorts op

inPorts (ComposePar ops) = foldl (++) [] $ scalePortsPerOp inPorts ops
inPorts (ComposeSeq ops) = scalePortsStreamLens (numFirings opHd) (inPorts opHd)
  where opHd = head ops
inPorts (ComposeFailure _ _) = []


oneOutSimplePort t = [T_Port "O" 1 t 2]
outPorts :: Op -> [PortType]
outPorts (Add t) = oneOutSimplePort t
outPorts (Sub t) = oneOutSimplePort t
outPorts (Mul t) = oneOutSimplePort t
outPorts (Div t) = oneOutSimplePort t
outPorts (MemRead t) = [T_Port "O" 1 t 1]
outPorts (MemWrite _) = []
-- go back to (sLen - ((w `ceilDiv` p) - 1)) for out stream length when 
-- including warmup and shutdown
outPorts (LineBuffer p w t) = [T_Port "O" (SWLen 1 ((w `ceilDiv` p) - 1)) (T_Array p (T_Array w t)) 2]
outPorts (Constant_Int ints) = [T_Port "O" baseWithNoWarmupSequenceLen (T_Array (length ints) T_Int) 1]
outPorts (Constant_Bit bits) = [T_Port "O" baseWithNoWarmupSequenceLen (T_Array (length bits) T_Bit) 1]
outPorts (SequenceArrayController _ (outSLen, outType)) = [T_Port "O" (SWLen outSLen 0) outType 2]

outPorts (MapOp par op) = duplicatePorts par (outPorts op)
outPorts (ReduceOp par numComb op) = map scaleSSLen $ outPorts op
  where scaleSSLen (T_Port name sLen tType pct) = T_Port name (multToSteadyState (numComb `ceilDiv` par) sLen) tType pct

outPorts (RegDelay _ op) = outPorts op

outPorts (ComposePar ops) = foldl (++) [] $ scalePortsPerOp outPorts ops
outPorts (ComposeSeq ops) = scalePortsStreamLens (numFirings opLst) (outPorts opLst)
  where opLst = last ops
outPorts (ComposeFailure _ _) = []

isComb :: a -> Bool
isComb (Add t) = True
isComb (Sub t) = True
isComb (Mul t) = True
isComb (Div t) = True
-- this is meaningless for this units that don't have both and input and output
isComb (MemRead _) = True
isComb (MemWrite _) = True
isComb (LineBuffer _ _ _) = True
isComb (Constant_Int _) = True
isComb (Constant_Bit _) = True
-- even if have sequential logic to store over multipel clocks,
-- always combinational path through for first clock
isComb (SequenceArrayController (inSLen, _) (outSLen, _)) = True

isComb (MapOp _ _ op) = isComb op
isComb (ReduceOp pEl totEl op) | pEl `mod` totEl == 0 = isComb op
isComb (ReduceOp _ _ op) = False

isComb (Underutil denom op) = isComb op
-- since pipelined, this doesn't affect clocks per stream
isComb (RegDelay _ op) = False

isComb (ComposePar ops) = length (filter isComb ops) > 0
isComb (ComposeSeq ops) = length (filter isComb ops) > 0
isComb (ComposeFailure _ _) = True


portThroughput :: Op -> PortType -> (TokenType, SteadyStateAndWarmupRatio)
portThroughput op (T_Port _ sLen tType _) = (tType, SWRatio sLen $ cps op)

inThroughput :: Op -> [(TokenType, SteadyStateAndWarmupRatio)]
inThroughput op = map (portThroughput) $ inPorts op

outThroughput :: Op -> [(TokenType, SteadyStateAndWarmupRatio)]
outThroughput op = map (portThroughput) $ outPorts op

-- SeqPortMismatch indicates couldn't do comopse as composeSeq requires 
-- all port types and latencies 
data ComposeResult = SeqPortMismatch | ParLatencyMismash | ComposeSuccess
  deriving (Eq, Show)

-- This is for making ComposeSeq
(|.|) :: Op -> Op -> Op
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
(|.|) op0 op1 = canComposeSeq op0 op1

-- This is for making ComposePar
(|&|) :: Maybe Op -> Maybe Op -> Maybe Op
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

-- only join two sequential nodes if token types match, ports do same number of tokens
-- over all firings and streams per firing, and if same number of clock cycles
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
