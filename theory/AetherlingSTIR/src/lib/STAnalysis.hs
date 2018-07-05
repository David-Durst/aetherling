{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module STAnalysis where
import STTypes
import STMetrics
import STAST
import Data.Bool

-- for wire space, only counting input wires, not outputs. This avoids
-- double counting
space :: Op -> OpsWireArea
space (Add t) = OWA (len t) (2 * len t)
space (Sub t) = space (Add t)
space (Mul t) = OWA (mulSpaceTimeIncreaser * len t) wireArea
  where OWA _ wireArea = space (Add t)
space (Div t) = OWA (divSpaceTimeIncreaser * len t) wireArea
  where OWA _ wireArea = space (Add t)
space (Max t) = space (Add t)
space (Min t) = space (Add t)
space (Ashr _ t) = OWA (len t) (len t)
space (Shl _ t) = space (Ashr 1 t)
space (Abs t) = OWA (len t) (len t)
space (Not t) = space (Abs t)
space (And t) = space (Add t)
space (Or t) = space (Add t)
space (XOr t) = space (Add t)
space Eq = space (Add T_Int)
space Neq = space (Add T_Int)
space Lt = space (Add T_Int)
space Leq = space (Add T_Int)
space Gt = space (Add T_Int)
space Geq = space (Add T_Int)


space (MemRead t) = OWA (len t) (len t)
space (MemWrite t) = OWA (len t) (len t)
-- need registers for storing intermediate values
-- registers account for wiring as some registers receive input wires,
-- others get wires from other registers
-- add |+| counterSpace (p `ceilDiv` w) when accounting for warmup counter
space (LineBuffer p w t) = registerSpace [T_Array (p + w - 1) t]
space (Constant_Int consts) = OWA (len (T_Array (length consts) T_Int)) 0
space (Constant_Bit consts) = OWA (len (T_Array (length consts) T_Bit)) 0
-- just a pass through, so will get removed by CoreIR
space (SequenceArrayController inPortsAndSLens outPortsAndSLens) |
  numPortsWithSLen1 == (length inPortsAndSLens + length outPortsAndSLens) = addId
  where
    numPortsWithSLen1 = length $ filter ((==) 1) $ map fst $ inPortsAndSLens ++ outPortsAndSLens
-- may need a more accurate approximate, but most conservative is storing
-- entire input
space (SequenceArrayController inPortsAndSLens _) = foldl (|+|) addId $ map elSpace inPortsAndSLens
  where elSpace (inSLen, inType) = registerSpace [inType] |* inSLen
-- since pass through with no logic and resulting ops will count input wire sizes, no need to account for any space here
space (DuplicateOutputs _ _) = addId

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
    PortThroughput _ (SWRatio (SWLen numSSMult _) (SWLen denomSSMult _)) = 
      portThroughput op $ head $ inPorts op

space (Underutil denom op) = space op |+| counterSpace denom
space (RegDelay dc op) = space op |+|
  ((registerSpace $ map pTType $ outPorts op) |* dc)

space (ComposePar ops) = foldl (|+|) addId $ map space ops
space (ComposeSeq ops) = foldl (|+|) addId $ map space ops
space (ComposeFailure _ _) = OWA (-1) (-1)

-- scaleCPS depending on if Op is combinational or not
scaleCPS :: Op -> Int -> SteadyStateAndWarmupLen
scaleCPS op n | isComb op = baseWithNoWarmupSequenceLen
scaleCPS op n = SWLen (ssMult * n) (wSub * n)
  where (SWLen ssMult wSub) = cps op

cps op = clocksPerSequence op
clocksPerSequence :: Op -> SteadyStateAndWarmupLen
registerCPS = baseWithNoWarmupSequenceLen
getAllSACSeqLens (SequenceArrayController inputs outputs) = map fst $ inputs ++ outputs
getAllSACSeqLens _ = undefined

clocksPerSequence (Add t) = baseWithNoWarmupSequenceLen
clocksPerSequence (Sub t) = baseWithNoWarmupSequenceLen
clocksPerSequence (Mul t) = baseWithNoWarmupSequenceLen
clocksPerSequence (Div t) = baseWithNoWarmupSequenceLen
clocksPerSequence (Max t) = baseWithNoWarmupSequenceLen
clocksPerSequence (Min t) = baseWithNoWarmupSequenceLen
clocksPerSequence (Ashr _ t) = baseWithNoWarmupSequenceLen
clocksPerSequence (Shl _ t) = baseWithNoWarmupSequenceLen
clocksPerSequence (Abs t) = baseWithNoWarmupSequenceLen
clocksPerSequence (Not t) = baseWithNoWarmupSequenceLen
clocksPerSequence (And t) = baseWithNoWarmupSequenceLen
clocksPerSequence (Or t) = baseWithNoWarmupSequenceLen
clocksPerSequence (XOr t) = baseWithNoWarmupSequenceLen
clocksPerSequence Eq = baseWithNoWarmupSequenceLen
clocksPerSequence Neq = baseWithNoWarmupSequenceLen
clocksPerSequence Lt = baseWithNoWarmupSequenceLen
clocksPerSequence Leq = baseWithNoWarmupSequenceLen
clocksPerSequence Gt = baseWithNoWarmupSequenceLen
clocksPerSequence Geq = baseWithNoWarmupSequenceLen

-- to what degree can we pipeline MemRead and MemWrite
clocksPerSequence (MemRead _) = baseWithNoWarmupSequenceLen
clocksPerSequence (MemWrite _) = baseWithNoWarmupSequenceLen
clocksPerSequence (LineBuffer p w _) = SWLen 1 0
clocksPerSequence (Constant_Int _) = baseWithNoWarmupSequenceLen
clocksPerSequence (Constant_Bit _) = baseWithNoWarmupSequenceLen
-- since one of the lengths must divide the other (as must be able to cleanly)
-- divide/group the input into the output, just take max as that is lcm
clocksPerSequence sac@(SequenceArrayController _ _) = SWLen (maximum $ getAllSACSeqLens sac) 0
clocksPerSequence (DuplicateOutputs _ _) = baseWithNoWarmupSequenceLen

clocksPerSequence (MapOp _ op) = cps op
-- always can pipeline. Just reset register every numComb/par if not fully parallel
clocksPerSequence (ReduceOp par numComb op) = SWLen (ssMult * (numComb `ceilDiv` par)) 
  -- need to add one here if not fully parallel for extra op that combines results
  -- over multiple cycles
  (wSub * (ceilLog par + (bool 1 0 (par == numComb))))
  where (SWLen ssMult wSub) = cps op

clocksPerSequence (Underutil denom op) = multToSteadyState denom $ clocksPerSequence op
-- since pipelined, this doesn't affect clocks per stream
clocksPerSequence (RegDelay _ op) = clocksPerSequence op

clocksPerSequence (ComposePar ops) = SWLen lcmSteadyState maxWarmup
  where 
    maxWarmup = maximum $ map (warmupSub . cps) ops
    -- 1 works as all integers for steady state >= 1
    lcmSteadyState = foldl lcm 1 $ map (steadyStateMultiplier . cps) ops
-- this depends on only wiring up things that have matching throughputs
clocksPerSequence (ComposeSeq ops) = SWLen lcmSteadyState sumWarmup
  where
    sumWarmup = sum $ map (warmupSub . cps) ops
    lcmSteadyState = foldl lcm 1 $ map (steadyStateMultiplier . cps) ops
clocksPerSequence (ComposeFailure _ _) = SWLen (-1) (-1)


registerInitialLatency = 1
initialLatency :: Op -> Int
initialLatency (Add t) = 1
initialLatency (Sub t) = 1
initialLatency (Mul t) = 1
initialLatency (Div t) = 1
initialLatency (Max t) = 1
initialLatency (Min t) = 1
initialLatency (Ashr _ t) = 1
initialLatency (Shl _ t) = 1
initialLatency (Abs t) = 1
initialLatency (Not t) = 1
initialLatency (And t) = 1
initialLatency (Or t) = 1
initialLatency (XOr t) = 1
initialLatency Eq = 1
initialLatency Neq = 1
initialLatency Lt = 1
initialLatency Leq = 1
initialLatency Gt = 1
initialLatency Geq = 1

initialLatency (MemRead _) = 1
initialLatency (MemWrite _) = 1
-- for each extra element in per clock, first output is 1 larger, but get 1
-- extra every clock building up to first output
initialLatency (LineBuffer p w _) = (w + p - 1) `ceilDiv` p
initialLatency (Constant_Int _) = 1
initialLatency (Constant_Bit _) = 1
initialLatency sac@(SequenceArrayController _ _) = foldl lcm 1 $ getAllSACSeqLens sac
initialLatency (DuplicateOutputs _ _) = 1

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
-- this is here to silence incomplete pattern warnings
getMultiOpCombGroupings _ = undefined

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
maxCombPath (Max t) = 1
maxCombPath (Min t) = 1
maxCombPath (Ashr _ t) = 1
maxCombPath (Shl _ t) = 1
maxCombPath (Abs t) = 1
maxCombPath (Not t) = 1
maxCombPath (And t) = 1
maxCombPath (Or t) = 1
maxCombPath (XOr t) = 1
maxCombPath Eq = 1
maxCombPath Neq = 1
maxCombPath Lt = 1
maxCombPath Leq = 1
maxCombPath Gt = 1
maxCombPath Geq = 1

maxCombPath (MemRead _) = 1
maxCombPath (MemWrite _) = 1
maxCombPath (LineBuffer _ _ _) = 1
maxCombPath (Constant_Int _) = 1
maxCombPath (Constant_Bit _) = 1
maxCombPath (SequenceArrayController _ _) = 1
maxCombPath (DuplicateOutputs _ _) = 1

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
util (Max t) = 1
util (Min t) = 1
util (Ashr _ t) = 1
util (Shl _ t) = 1
util (Abs t) = 1
util (Not t) = 1
util (And t) = 1
util (Or t) = 1
util (XOr t) = 1
util Eq = 1
util Neq = 1
util Lt = 1
util Leq = 1
util Gt = 1
util Geq = 1

util (MemRead _) = 1
util (MemWrite _) = 1
util (LineBuffer _ _ _) = 1
util (Constant_Int _) = 1
util (Constant_Bit _) = 1
util (SequenceArrayController _ _) = 1
util (DuplicateOutputs _ _) = 1

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

unionPorts :: (Op -> [PortType]) -> [Op] -> [PortType]
unionPorts portsGetter ops = foldl (++) [] $ map portsGetter ops

-- for using some operator over a list of ints to combine all the warmups in one
combineAllWarmups ops summarizer portGetter = summarizer 
  -- can take head as assuming that all ports have same warmup for a module
  $ map (warmupSub . pSeqLen . head . portGetter) ops

-- Helper for in and out ports of composeSeq
-- for each op in the list ops, get all the in or out ports 
-- Then, create a scaling factor for each op, returning flat list with
-- a copy of the scaling factor for an op replicated for each of the ops ports
getSSScalingsForEachPortOfEachOp :: Op -> [Op] -> (Op -> [PortType]) -> [Int]
getSSScalingsForEachPortOfEachOp containerOp ops portGetter = ssScalings
  where
    -- given one op, get the scaling factor for its ports
    -- will always get int here with right rounding as CPS of overall composePar
    -- is multiple of cps of each op
    ssScaling op = (steadyStateMultiplier $ cps containerOp) `ceilDiv` 
      (steadyStateMultiplier $ cps op)
    -- given one op, get a scaling factor for each of its ports
    -- note: all will be same, just need duplicates
    ssScaleFactorsForOp op = replicate (length $ portGetter op) (ssScaling op)
    -- scaling factors for all ports of all ops
    ssScalings = foldl (++) [] $ map ssScaleFactorsForOp ops

-- update the sequence lengths of a list of ports, where all must have
-- same warmup and can be scaled to different sequence lengths
scalePorts :: [Int] -> Int -> [PortType] -> [PortType]
scalePorts ssScalings newWarmup ports = map updatePort $ zip ports ssScalings
  where
    updatePort (T_Port name (SWLen origSteadyState _) tType pct, ssScaling) = 
      T_Port name (SWLen (origSteadyState * ssScaling) newWarmup) tType pct

oneInSimplePort t = [T_Port "I" baseWithNoWarmupSequenceLen t 1]
twoInSimplePorts t = [T_Port "I0" baseWithNoWarmupSequenceLen t 1, 
  T_Port "I1" baseWithNoWarmupSequenceLen t 1]

-- inPorts and outPorts handle the sequence lengths because each port can 
-- have its own
inPorts :: Op -> [PortType]
inPorts (Add t) = twoInSimplePorts t
inPorts (Sub t) = twoInSimplePorts t
inPorts (Mul t) = twoInSimplePorts t
inPorts (Div t) = twoInSimplePorts t
inPorts (Max t) = twoInSimplePorts t
inPorts (Min t) = twoInSimplePorts t
inPorts (Ashr _ t) = oneInSimplePort t
inPorts (Shl _ t) = oneInSimplePort t
inPorts (Abs t) = oneInSimplePort t
inPorts (Not t) = oneInSimplePort t
inPorts (And t) = twoInSimplePorts t
inPorts (Or t) = twoInSimplePorts t
inPorts (XOr t) = twoInSimplePorts t
inPorts Eq = twoInSimplePorts T_Int
inPorts Neq = twoInSimplePorts T_Int
inPorts Lt = twoInSimplePorts T_Int
inPorts Leq = twoInSimplePorts T_Int
inPorts Gt = twoInSimplePorts T_Int
inPorts Geq = twoInSimplePorts T_Int

inPorts (MemRead _) = []
inPorts (MemWrite t) = [T_Port "I" baseWithNoWarmupSequenceLen t 1]
-- 2 as it goes straight through LB
inPorts (LineBuffer p w t) = [T_Port "I" (SWLen 1 (w + p - 2)) (T_Array p t) 2]
inPorts (Constant_Int _) = []
inPorts (Constant_Bit _) = []

inPorts (SequenceArrayController inputs _) = snd $ foldl makePort (0, []) inputs
  where
    makePort (n, ports) (sLen, tType) = (n+1, ports ++ [T_Port ("I" ++ show n) (SWLen sLen 0) tType 2])
-- for in ports, no duplicates
inPorts (DuplicateOutputs _ op) = inPorts op

inPorts (MapOp par op) = renamePorts "I" $ duplicatePorts par (inPorts op)
-- take the first port of the op and duplicate it par times, don't duplicate both
-- ports of reducer as reducing numComb things in total, not per port
inPorts (ReduceOp par numComb op) = renamePorts "I" $ map scaleSSForReduce $ duplicatePorts par $
  portToDuplicate $ inPorts op
  where 
    scaleSSForReduce (T_Port name (SWLen origMultSS wSub) tType pct) = T_Port 
      name (SWLen (origMultSS * (numComb `ceilDiv` par)) wSub) tType pct
    -- renamed the port to duplicate as op ports named for two input ports
    -- and this only has one input port
    portToDuplicate ((T_Port _ sLen tType pct):_) = [T_Port "I" sLen tType pct]
    portToDuplicate [] = []

inPorts (Underutil _ op) = inPorts op
inPorts (RegDelay _ op) = inPorts op

inPorts cPar@(ComposePar ops) = renamePorts "I" $ scalePorts
  (getSSScalingsForEachPortOfEachOp cPar ops inPorts) 
  (combineAllWarmups ops maximum inPorts) (unionPorts inPorts ops)
-- this depends on only wiring up things that have matching throughputs
inPorts (ComposeSeq []) = []
inPorts cSeq@(ComposeSeq ops@(hd:_)) = renamePorts "I" $
  scalePorts (replicate (length $ inPorts hd) ssScaling) 
  (combineAllWarmups ops sum inPorts) (inPorts hd)
  where
    -- the first op in the seq which we're gonna scale the input ports of
    ssScaling = (steadyStateMultiplier $ cps cSeq) `ceilDiv` 
      (steadyStateMultiplier $ cps hd)
inPorts (ComposeFailure _ _) = []


oneOutSimplePort t = [T_Port "O" baseWithNoWarmupSequenceLen t 1]
outPorts :: Op -> [PortType]
outPorts (Add t) = oneOutSimplePort t
outPorts (Sub t) = oneOutSimplePort t
outPorts (Mul t) = oneOutSimplePort t
outPorts (Div t) = oneOutSimplePort t
outPorts (Max t) = oneOutSimplePort t
outPorts (Min t) = oneOutSimplePort t
outPorts (Ashr _ t) = oneOutSimplePort t
outPorts (Shl _ t) = oneOutSimplePort t
outPorts (Abs t) = oneOutSimplePort t
outPorts (Not t) = oneOutSimplePort t
outPorts (And t) = oneOutSimplePort t
outPorts (Or t) = oneOutSimplePort t
outPorts (XOr t) = oneOutSimplePort t
outPorts Eq = oneOutSimplePort T_Bit
outPorts Neq  = oneOutSimplePort T_Bit
outPorts Lt = oneOutSimplePort T_Bit
outPorts Leq = oneOutSimplePort T_Bit
outPorts Gt = oneOutSimplePort T_Bit
outPorts Geq = oneOutSimplePort T_Bit

outPorts (MemRead t) = oneOutSimplePort t
outPorts (MemWrite _) = []
-- go back to (sLen - ((w `ceilDiv` p) - 1)) for out stream length when 
-- including warmup and shutdown
outPorts (LineBuffer p w t) = [T_Port "O" (SWLen 1 (w + p - 2)) (T_Array p (T_Array w t)) 2]
outPorts (Constant_Int ints) = [T_Port "O" baseWithNoWarmupSequenceLen (T_Array (length ints) T_Int) 1]
outPorts (Constant_Bit bits) = [T_Port "O" baseWithNoWarmupSequenceLen (T_Array (length bits) T_Bit) 1]

outPorts (SequenceArrayController _ outputs) = snd $ foldl makePort (0, []) outputs
  where
    makePort (n, ports) (sLen, tType) = (n+1, ports ++ [T_Port ("O" ++ show n) (SWLen sLen 0) tType 2])
outPorts (DuplicateOutputs n op) = renamePorts "O" $ foldl (++) [] $ replicate n $ outPorts op

outPorts (MapOp par op) = renamePorts "O" $ duplicatePorts par (outPorts op)
outPorts (ReduceOp _ _ op) = renamePorts "O" $ outPorts op

outPorts (Underutil _ op) = outPorts op
outPorts (RegDelay _ op) = outPorts op

outPorts cPar@(ComposePar ops) = renamePorts "O" $ scalePorts
  (getSSScalingsForEachPortOfEachOp cPar ops outPorts) 
  (combineAllWarmups ops maximum outPorts) (unionPorts outPorts ops)
outPorts (ComposeSeq []) = []
outPorts cSeq@(ComposeSeq ops) = renamePorts "O" $
  scalePorts (replicate (length $ outPorts lastOp) ssScaling) 
  (combineAllWarmups ops sum outPorts) (outPorts lastOp)
  where
    lastOp = last ops
    -- the first op in the seq which we're gonna scale the input ports of
    ssScaling = (steadyStateMultiplier $ cps cSeq) `ceilDiv` 
      (steadyStateMultiplier $ cps lastOp)
outPorts (ComposeFailure _ _) = []

isComb :: Op -> Bool
isComb (Add t) = True
isComb (Sub t) = True
isComb (Mul t) = True
isComb (Div t) = True
isComb (Max t) = True
isComb (Min t) = True
isComb (Ashr _ t) = True
isComb (Shl _ t) = True
isComb (Abs t) = True
isComb (Not t) = True
isComb (And t) = True
isComb (Or t) = True
isComb (XOr t) = True
isComb Eq = True
isComb Neq = True
isComb Lt = True
isComb Leq = True
isComb Gt = True
isComb Geq = True

-- this is meaningless for this units that don't have both and input and output
isComb (MemRead _) = True
isComb (MemWrite _) = True
isComb (LineBuffer _ _ _) = True
isComb (Constant_Int _) = True
isComb (Constant_Bit _) = True
-- even if have sequential logic to store over multipel clocks,
-- always combinational path through for first clock
isComb (SequenceArrayController _ _) = True
isComb (DuplicateOutputs _ _) = True

isComb (MapOp _ op) = isComb op
isComb (ReduceOp par numComb op) | par == numComb = isComb op
isComb (ReduceOp _ _ op) = False

isComb (Underutil denom op) = isComb op
-- since pipelined, this doesn't affect clocks per stream
isComb (RegDelay _ op) = False

isComb (ComposePar ops) = length (filter isComb ops) > 0
isComb (ComposeSeq ops) = length (filter isComb ops) > 0
isComb (ComposeFailure _ _) = True


portThroughput :: Op -> PortType -> PortThroughput
portThroughput op (T_Port _ sLen tType _) = PortThroughput tType (SWRatio sLen $ cps op)

-- This is throughput only considering steady state
ssPortThroughput op (T_Port _ (SWLen ssMult _) tType _) = PortThroughput tType
  (SWRatio (SWLen ssMult 0) $ cps op)

inThroughput :: Op -> [PortThroughput]
inThroughput op = map (portThroughput op) $ inPorts op

outThroughput :: Op -> [PortThroughput]
outThroughput op = map (portThroughput op) $ outPorts op
