{-# LANGUAGE StandaloneDeriving, ExistentialQuantification #-}
module SpaceTime.STOps where
import SpaceTime.STTypes
import SpaceTime.STMetrics
import Data.Typeable

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

twoInSimplePorts t = [T_Port "I0" 1 t, T_Port "I1" 1 t]
oneOutSimplePort t = [T_Port "O" 1 t]

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
  |+| (counterSpace $ seqTime $ time rOp)
  where reduceTreeSpace = space (ReduceOp pEl pEl op)

space iOp@(IterOp numIters _ op) = space op |+| counterSpace (seqTime $ time iOp)
space (RegDelay ds dc op) = counterSpace dc |+| 
  ((registerSpace $ map pTType $ inPortsType op) |* (ds + (numStages $ pipelineTime op))) |+| space op

space (ComposePar ops) = foldl (|+|) addId $ map space ops
space (ComposeSeq ops) = foldl (|+|) addId $ map space ops
space (ComposeFailure _ _) = 0

cps op = clocksPerStream op
clocksPerStream :: Op -> Int

clocksPerStream (Add t) = 1
clocksPerStream (Sub t) = 1
clocksPerStream (Mul t) = 1
clocksPerStream (Div t) = 1
clocksPerStream (MemRead _) = rwTime
clocksPerStream (MemWrite _) = rwTime
clocksPerStream (LineBuffer _ _ _) = 1
clocksPerStream (Constant_Int _ _) = 1
clocksPerStream (Constant_Bit _ _) = 1
clocksPerStream (StreamArrayController (inSLen, _) (outSLen, _)) | 
  inSLen == 1 && outSLen == 1 = 1
clocksPerStream (StreamArrayController (inSLen, _) (outSLen, _)) = lcm inSLen outSLen

clocksPerStream (MapOp pEl totEl op) = replicateTimeOverStream (totEl `ceilDiv` pEl) (time op)
clocksPerStream (ReduceOp pEl totEl op) | pEl == totEl = (time op) |* (ceilLog pEl)
clocksPerStream (ReduceOp pEl totEl op) =
  replicateclocksPerStreamOverStream (totEl `ceilDiv` pEl) (reduceTreeTime |+| (time op) |+| registerTime)
  where reduceTreeclocksPerStream = time (ReduceOp pEl pEl op)

clocksPerStream (IterOp numIters _ op) = replicateTimeOverStream numIters (time op)
clocksPerStream (RegDelay ds dc op) = time op |+| 
  SCclocksPerStream (dc * (ds + (numStages $ pipelineTime op))) 0

-- what's correct here for streamarraycontroller for non-zero seqclocksPerStream?
-- for everythign else, emitting something every clock if sequential, and 
-- no impact on clocks if combinational
pipelineTime op = PTime 1 1

util (Add t) = 1
util (Sub t) = 1
util (Mul t) = 1
util (Div t) = 1
util (MemRead _) = 1
util (MemWrite _) = 1
util (LineBuffer _ _ _) = 1
util (Constant_Int _ _) = 1
util (Constant_Bit _ _) = 1
util (StreamArrayController (inSLen, _) (outSLen, _)) | 

util (MapOp _ _ op) = util op
util (ReduceOp _ _ op) = util op

util (IterOp totalIters usedIters op) = util op * (fromIntegral usedIters) /
  (fromIntegral $ totalIters)
-- ignoring dleay stages for now, need to come back to them later
-- I think no underutilziation here, as registers being used for pipelining
util (RegDelay _ dc op) = util op


inPortsType (Add t) = twoInSimplePorts t
inPortsType (Sub t) = twoInSimplePorts t
inPortsType (Mul t) = twoInSimplePorts t
inPortsType (Div t) = twoInSimplePorts t
inPortsType (MemRead _) = []
inPortsType (MemWrite t) = [T_Port "I" 1 t]
inPortsType (LineBuffer p _ t) = [T_Port "I" 1 (T_Array p t)]
inPortsType (Constant_Int _ _) = []
inPortsType (Constant_Bit _ _) = []
inPortsType (StreamArrayController (inSLen, inType) _) = [T_Port "I" inSLen inType]

inPortsType (MapOp pEl totEl op) = duplicatePorts pEl $
  scalePortsStreamLens (totEl `ceilDiv` pEl) (inPortsType op)
inPortsType (ReduceOp pEl totEl op) = duplicatePorts pEl $
-- can just take head as can only reduce binary operators
-- parallelism means apply binary to pEl at a time
  scalePortsStreamLens (totEl `ceilDiv` pEl) [head $ inPortsType op]

inPortsType (IterOp _ usedIters op) = scalePortsStreamLens usedIters $ inPortsType op
inPortsType (RegDelay _ _ op) = inPortsType op


outPortsType (Add t) = oneOutSimplePort t
outPortsType (Sub t) = oneOutSimplePort t
outPortsType (Mul t) = oneOutSimplePort t
outPortsType (Div t) = oneOutSimplePort t
outPortsType (MemRead t) = [T_Port "O" 1 t]
outPortsType (MemWrite _) = []
-- go back to (sLen - ((w `ceilDiv` p) - 1)) for out stream length when 
-- including warmup and shutdown
outPortsType (LineBuffer p w t) = [T_Port "O" 1 (T_Array p (T_Array w t))]
outPortsType (Constant_Int n ints) = [T_Port "O" n (T_Array (length ints) T_Int)]
outPortsType (Constant_Bit n bits) = [T_Port "O" n (T_Array (length bits) T_Bit)]
outPortsType (StreamArrayController _ (outSLen, outType)) = [T_Port "O" outSLen outType]

outPortsType (MapOp pEl totEl op) = duplicatePorts pEl $
  scalePortsStreamLens (totEl `ceilDiv` pEl) (outPortsType op)
outPortsType (ReduceOp _ _ op) = outPortsType op

outPortsType (IterOp _ usedIters op) = scalePortsStreamLens usedIters $ outPortsType op
outPortsType (RegDelay _ _ op) = outPortsType op

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
