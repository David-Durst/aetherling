{-# LANGUAGE StandaloneDeriving, ExistentialQuantification #-}
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
  -- Array is constant produced, int is stream length
  | Constant_Int {intConstStreamLen :: Int, intConstProduced :: [Int]}
  -- Array is constant produced, int is stream length
  | Constant_Bit {bitConstStreamLen :: Int, bitConstProduced [Bool]}
  -- first pair is input stream length and tokens per stream element, second is output
  | StreamArrayController (Int, TokenType) (Int, TokenType)

  -- HIGHER ORDER OPS
  | MapOp {mapParallelism :: Int, mapStreamLen :: Int, mappedOp :: Op}
  | ReduceOp {reduceParallelism :: Int, reduceStreamLen :: Int, reducedOp :: Op}

  -- ITERATION AND UNDERUTILIZATION OPS
  | IterOp {numIterations :: Int, iteratedOp :: Op}
  -- run underOp at 1 / utilDenominator CPS
  | Underutil {utilDenominator :: int, underutilizedOp :: Op}
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
space (Constant_Int n consts) = OWA (len (T_Array n T_Int)) 0
space (Constant_Bit n consts) = OWA (len (T_Array n T_Bit)) 0
-- just a pass through, so will get removed by CoreIR
space (StreamArrayController (inSLen, _) (outSLen, _)) | 
  inSLen == 1 && outSLen == 1 = addId
-- may need a more accurate approximate, but most conservative is storing
-- entire input
space (StreamArrayController (inSLen, inType) _) = registerSpace [inType] |* inSLen

-- area of parallel map is area of all the copies
space (MapOp pEl _ op) = (space op) |* pEl
-- area of reduce is area of reduce tree, with area for register for partial
-- results and counter for tracking iteration time if input is stream of more
-- than what is passed in one clock
space (ReduceOp pEl totEl op) | pEl == totEl = (space op) |* (pEl - 1)
space rOp@(ReduceOp pEl _ op) =
  reduceTreeSpace |+| (space op) |+| (registerSpace $ map pTType $ outPortsType op)
  |+| (counterSpace $ cps rOp)
  where reduceTreeSpace = space (ReduceOp pEl pEl op)

space (IterOp numIters op) = space op |+| counterSpace numIters
space (Underutil denom op) = space op |+| counterSpace denom
space (RegDelay dc op) = space op |+|
  ((registerSpace $ map pTType $ outPortsType op) |* dc)

space (ComposePar ops) = foldl (|+|) addId $ map space ops
space (ComposeSeq ops) = foldl (|+|) addId $ map space ops
space (ComposeFailure _ _) = 0

-- scaleCPS depending on if Op is combinational or not
scaleCPS :: Op -> Int -> Int
scaleCPS op n | isComb = 1
scaleCPS op n = cps op * n

cps op = clocksPerStream op
clocksPerStream :: Op -> Int
registerCPS = 1

clocksPerStream (Add t) = 1
clocksPerStream (Sub t) = 1
clocksPerStream (Mul t) = 1
clocksPerStream (Div t) = 1
-- to what degree can we pipeline MemRead and MemWrite
clocksPerStream (MemRead _) = rwTime
clocksPerStream (MemWrite _) = rwTime
clocksPerStream (LineBuffer _ _ _) = 1
clocksPerStream (Constant_Int _ _) = 1
clocksPerStream (Constant_Bit _ _) = 1
clocksPerStream (StreamArrayController (inSLen, _) (outSLen, _)) = lcm inSLen outSLen

clocksPerStream (MapOp pEl totEl op) = cps op * (totEl `ceilDiv` pEl)
-- can only pipeline a reduce if its fully parallel
clocksPerStream (ReduceOp pEl totEl op) | pEl `mod` totEl == 0 =
  scaleCPS op (ceilLog totEl)
clocksPerStream (ReduceOp pEl totEl op) =
  (totEl `ceilDiv` pEl) * (reduceTreeCPS + cps op + registerCPS)
  where 
    reduceTreeCPS = cps (ReduceOp pEl pEl op)
    -- op adds nothing if its combinational, its CPS else
    opCPS = bool 0 (cps op) (isComb op)

clocksPerStream (IterOp numIters op) = numIters * clocksPerStream op
clocksPerStream (Underutil denom op) = clocksPerStream op * denom
-- since pipelined, this doesn't affect clocks per stream
clocksPerStream (RegDelay _ op) = clocksPerStream op

-- this depends the constructors verifying that only composing valid things
-- see the document for what is valid
clocksPerStream (ComposePar (hd:tl)) = cps hd
clocksPerStream (ComposeSeq (hd:tl)) = cps hd
clocksPerStream (ComposeFailure _ _) = 0


registerLatency = 1
latency :: a -> Int
latency (Add t) = 1
latency (Sub t) = 1
latency (Mul t) = 1
latency (Div t) = 1
latency (MemRead _) = 1
latency (MemWrite _) = 1
-- for each extra element in per clock, first output is 1 larger, but get 1
-- extra every clock building up to first output
latency (LineBuffer p w _) = (w + p - 1) / p
latency (Constant_Int _ _) = 1
latency (Constant_Bit _ _) = 1
latency (StreamArrayController (inSLen, _) (outSLen, _)) = lcm inSLen outSLen

latency (MapOp _ _ op) = latency op
latency (ReduceOp pEl totEl op) | pEl `mod` totEl == 0 && isComb op = 1
latency (ReduceOp pEl totEl op) | pEl `mod` totEl == 0 = latency op * (totEl pEl)
latency (ReduceOp pEl totEl op) =
  (totEl `ceilDiv` pEl) * (reduceTreeLatency + latency op + registerLatency)
  where 
    reduceTreeCPS = latency (ReduceOp pEl pEl op)
    -- op adds nothing if its combinational, its CPS else
    opCPS = bool 0 (latency op) (isComb op)


latency (IterOp numIters op) = latency op
latency (Underutil denom op) = latency op
-- since pipelined, this doesn't affect clocks per stream
latency (RegDelay dc op) = latency op + dc

latency (ComposePar ops) = maximum $ map latency ops
-- latency is 1 if all elemetns are combintional, sum of latencies of sequential
-- elements otherwise
latency (ComposeSeq ops) = bool combinationalLatency sequentialLatency
  (sequentialLatency > 0)
  where 
    combinationalLatency = 1
    sequentialLatency = foldl (+) 0 $ map latency $ filter (not . isComb) ops
latency (ComposeFailure _ _) = 0


maxCombPath :: a -> Float
maxCombPath (Add t) = 1
maxCombPath (Sub t) = 1
maxCombPath (Mul t) = 1
maxCombPath (Div t) = 1
maxCombPath (MemRead _) = 1
maxCombPath (MemWrite _) = 1
maxCombPath (LineBuffer _ _ _) = 1
maxCombPath (Constant_Int _ _) = 1
maxCombPath (Constant_Bit _ _) = 1
maxCombPath (StreamArrayController (inSLen, _) (outSLen, _)) = 1

maxCombPath (MapOp _ _ op) = util op
maxCombPath (ReduceOp _ _ op) = util op

maxCombPath (IterOp numIters op) = util op
maxCombPath (Underutil denom op) = util op / denom
-- since pipelined, this doesn't affect clocks per stream
maxCombPath (RegDelay _ op) = util op

maxCombPath (ComposePar ops) = utilWeightedByArea ops
maxCombPath (ComposeSeq ops) = utilWeightedByArea ops
maxCombPath (ComposeFailure _ _) = 0


util :: a -> Float
util (Add t) = 1
util (Sub t) = 1
util (Mul t) = 1
util (Div t) = 1
util (MemRead _) = 1
util (MemWrite _) = 1
util (LineBuffer _ _ _) = 1
util (Constant_Int _ _) = 1
util (Constant_Bit _ _) = 1
util (StreamArrayController (inSLen, _) (outSLen, _)) = 1

util (MapOp _ _ op) = util op
util (ReduceOp _ _ op) = util op

util (IterOp numIters op) = util op
util (Underutil denom op) = util op / denom
-- since pipelined, this doesn't affect clocks per stream
util (RegDelay _ op) = util op

util (ComposePar ops) = utilWeightedByArea ops
util (ComposeSeq ops) = utilWeightedByArea ops
util (ComposeFailure _ _) = 0
-- is there a better utilization than weighted by operator area
utilWeightedByArea :: (SpaceTime a) => [a] -> Float
utilWeightedByArea ops = unnormalizedUtil / totalArea
    where 
      unnormalizedUtil = foldl (+) 0 $
        map (\op -> (fromIntegral $ opsArea $ space op) * (util op)) ops
      totalArea = foldl (+) 0 $ map (fromIntegral . opsArea . space) ops


twoInSimplePorts t = [T_Port "I0" 1 t 2, T_Port "I1" 1 t 2]
inPorts :: Op -> [T_Port]
inPorts (Add t) = twoInSimplePorts t
inPorts (Sub t) = twoInSimplePorts t
inPorts (Mul t) = twoInSimplePorts t
inPorts (Div t) = twoInSimplePorts t
inPorts (MemRead _) = []
inPorts (MemWrite t) = [T_Port "I" 1 t 1]
-- 2 as it goes straight through LB
inPorts (LineBuffer p _ t) = [T_Port "I" 1 (T_Array p t) 2]
inPorts (Constant_Int _ _) = []
inPorts (Constant_Bit _ _) = []
inPorts (StreamArrayController (inSLen, inType) _) = [T_Port "I" inSLen inType 2]

inPorts (MapOp pEl totEl op) = duplicatePorts pEl $
  scalePortsStreamLens (totEl `ceilDiv` pEl) (inPorts op)
inPorts (ReduceOp pEl totEl op) = duplicatePorts pEl $
-- can just take head as can only reduce binary operators
-- parallelism means apply binary to pEl at a time
  scalePortsStreamLens (totEl `ceilDiv` pEl) [head $ inPorts op]

inPorts (IterOp _ usedIters op) = scalePortsStreamLens usedIters $ inPorts op
inPorts (RegDelay _ _ op) = inPorts op

inPorts (ComposePar ops) = foldl (++) [] $ scalePortsPerOp inPorts ops
inPorts (ComposeSeq ops) = scalePortsStreamLens (numFirings opHd) (inPorts opHd)
  where opHd = head ops
inPorts (ComposeFailure _ _) = []


oneOutSimplePort t = [T_Port "O" 1 t 2]
outPorts :: Op -> [T_Port]
outPorts (Add t) = oneOutSimplePort t
outPorts (Sub t) = oneOutSimplePort t
outPorts (Mul t) = oneOutSimplePort t
outPorts (Div t) = oneOutSimplePort t
outPorts (MemRead t) = [T_Port "O" 1 t 1]
outPorts (MemWrite _) = []
-- go back to (sLen - ((w `ceilDiv` p) - 1)) for out stream length when 
-- including warmup and shutdown
outPorts (LineBuffer p w t) = [T_Port "O" 1 (T_Array p (T_Array w t)) 2]
outPorts (Constant_Int n ints) = [T_Port "O" n (T_Array (length ints) T_Int) 1]
outPorts (Constant_Bit n bits) = [T_Port "O" n (T_Array (length bits) T_Bit) 1]
outPorts (StreamArrayController _ (outSLen, outType)) = [T_Port "O" outSLen outType 2]

outPorts (MapOp pEl totEl op) = duplicatePorts pEl $
  scalePortsStreamLens (totEl `ceilDiv` pEl) (outPorts op)
outPorts (ReduceOp _ _ op) = outPorts op

outPorts (IterOp numIters op) = scalePortsStreamLens numIters $ outPorts op
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
isComb (Constant_Int _ _) = True
isComb (Constant_Bit _ _) = True
-- even if have sequential logic to store over multipel clocks,
-- always combinational path through for first clock
isComb (StreamArrayController (inSLen, _) (outSLen, _)) = True

isComb (MapOp _ _ op) = isComb op
isComb (ReduceOp pEl totEl op) | pEl `mod` totEl == 0 = isComb op
isComb (ReduceOp _ _ op) = False

isComb (IterOp numIters op) = isComb op
isComb (Underutil denom op) = isComb op
-- since pipelined, this doesn't affect clocks per stream
isComb (RegDelay _ op) = False

isComb (ComposePar ops) = length (filter isComb ops) > 0
isComb (ComposeSeq ops) = length (filter isComb ops) > 0
isComb (ComposeFailure _ _) = True


portThroughput :: Op -> T_Port -> (TokenType, Ratio)
portThroughput op (T_Port _ sLen tType _) = (tType, sLen % cps op)

inThroughput :: Op -> [(TokenType, Ratio)]
inThroughput op = map (portThroughput) $ inPorts op

outThroughput :: Op -> [(TokenType, Ratio)]
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
  | canComposeSeq op1 op0 == ComposeSuccess = $ ComposeSeq $ ops1 ++ ops0
(|.|) (op0@(ComposeSeq ops0)) (op1) | canComposeSeq op1 op0 == ComposeSuccess =
  $ ComposeSeq $ [op1] ++ ops0
(|.|) (op0) (op1@(ComposeSeq ops1)) | canComposeSeq op1 op0 == ComposeSuccess =
  $ ComposeSeq $ ops1 ++ [op0]
(|.|) (op0) (op1) | canComposeSeq op1 op0 == ComposeSuccess =
  $ ComposeSeq $ [op1] ++ [op0]
(|.|) op0 op1 = canComposeSeq op0 op1

-- This is for making ComposePar
(|&|) :: Maybe Compose -> Maybe Compose -> Maybe Compose
(|&|) (op0@(ComposePar ops0)) (op1@(ComposePar ops1)) | canComposePar op1 op0 == ComposeSuccess =
  $ ComposePar $ ops0 ++ ops1
(|&|) (op0@(ComposePar ops0)) (op1) | canComposePar op1 op0 == ComposeSuccess =
  $ ComposePar $ [op1] ++ ops0
(|&|) (op0) (op1@(ComposePar ops1)) | canComposePar op1 op0 == ComposeSuccess =
  $ ComposePar $ ops1 ++ [op0]
(|&|) (op0) (op1) | canComposePar op1 op0 == ComposeSuccess =
  $ ComposePar $ [op1] ++ [op0]
(|&|) op0 op1 = canComposePar op0 op1

-- This is in same spirit as Monad's >>=, kinda abusing notation
-- It's |.| in reverse so that can create pipelines in right order
(|>>=|) :: Op -> Op -> Op
(|>>=|) op0 op1 = op1 |.| op0

canComposeSeq :: (SpaceTime a) => a -> a -> Bool

-- only join two sequential nodes if token types match, ports do same number of tokens
-- over all firings and streams per firing, and if same number of clock cycles
canComposeSeq op0 op1 | (seqTime . time) op0 > 0 && (seqTime . time) op1 > 0 =
  -- this checks both token types and numTokens over all firing/stream combos
  outPortsType op0 == inPortsType op1 && 
  (numClocks . pipelineTime) op0 == (numClocks . pipelineTime) op1

-- can join a combinational node with another node if they do the same amount
-- every clock cycle
canComposeSeq op0 op1 = ((map pTType) . outPortsType) op0 ==
  ((map pTType) . inPortsType) op1

canComposePar :: (SpaceTime a) => a -> a -> Bool
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
portsScaledByFiringPerOp :: (SpaceTime a) => (a -> [PortType]) -> [a] -> [[PortType]]
portsScaledByFiringPerOp portGetter ops = map scalePerOp ops
  where scalePerOp op = scalePortsStreamLens (numFirings op) $ portGetter op
