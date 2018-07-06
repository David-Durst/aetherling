{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module STSimulate where
import STTypes
import STMetrics
import STAST
import STAnalysis
import Data.Bool
import Data.Bits
import Data.List

-- High level simulator for Aetherling pipelines (Op instances).
-- Useful for verifying that the logic of the circuit is correct, but
-- doesn't simulate the actual hardware implementation.
--
-- Input: list of lists of ValueType. Each list entry of the outer
-- list corresponds to the inputs of one clock cycle. These inner list
-- entries are a list of ValueType, with each ValueType corresponding
-- in order to one input port of the simulated Op.
--
-- Memory input: list of lists of ValueType. The inner lists are
-- "tapes" of input corresponding to one MemRead, which outputs the
-- tape's values sequentially. Index i of the outer list corresponds
-- to the input for the ith MemRead, which are numbered in the order
-- they would be visited by a depth-first search (DFS) of the AST.
--
-- Note: The port input and memory inputs are sort of "transposed"
-- unfortunately. The issue is that due to underutil memory
-- reads/writes don't always happen every cycle so we can't input them
-- the way that the port inputs are formatted.
--
-- Output: Tuple of [[ValueType]], [[ValueType]]. The first is the output
-- of the simulated op's output ports, in the same format as the input.
-- The second is the output of all MemWrites, in the same format as
-- input memory, and same numbering scheme.
-- TODO: More convenient error messages?
simulateHighLevel :: Op -> [[ValueType]] -> [[ValueType]]
    -> ([[ValueType]], [[ValueType]])
-- Check that the types match, then delegate to simhl implementation.
-- 1 is the underutil denominator.
simulateHighLevel op portInputs memoryInputs = do {
        let portOutputsAndState = simhl op portInputs
                (SimhlState (simhlCheckInputs 0 op portInputs)
                1 memoryInputs 0 [])
       ;
       (fst portOutputsAndState, simhlMemoryOut $ snd portOutputsAndState)
}

-- Inspect the inPorts of op and see if they match the sequences of ValueType
-- passed by the user. (Integer argument used to keep track of which
-- list index the error was at).
simhlCheckInputs :: Integer -> Op -> [[ValueType]] -> Bool
simhlCheckInputs clkIndex op [] = True
simhlCheckInputs clkIndex op (input:inputs) =
    if length input == length(inPorts op) then
        if simhlCheckPorts clkIndex 0 op (inPorts op) input then
            simhlCheckInputs (clkIndex+1) op inputs
        else
            error "Aetherling internal error: simhlCheckPorts returned False."
    else
        error("At clock cycle "
           ++ show clkIndex
           ++ " got "
           ++ (show $ length input)
           ++ " ValueType inputs but Op instance "
           ++ (show op)
           ++ " has "
           ++ (show $ length $ inPorts op)
           ++ " input ports.")

simhlCheckPorts :: Integer -> Integer -> Op -> [PortType] -> [ValueType] -> Bool
simhlCheckPorts clkIndex portIndex op [] [] = True
simhlCheckPorts clkIndex portIndex op _ [] =
    error "Aetherling internal error: input and port count should match by now."
simhlCheckPorts clkIndex portIndex op [] _ =
    error "Aetherling internal error: input and port count should match by now."
simhlCheckPorts clkIndex portIndex op (port:ports) (value:values) =
    if value == V_Unit || vtTypesMatch value (pTType port) then
        simhlCheckPorts clkIndex (portIndex+1) op ports values
    else
        error("At clock cycle "
           ++ show clkIndex
           ++ ", input number "
           ++ show portIndex
           ++ " ("
           ++ show value
           ++ ") does not match input type "
           ++ show (pTType port)
           ++ " expected by Op instance "
           ++ show op)

data SimhlState = SimhlState {
    simhlTypesMatch :: Bool,
    simhlUnderutil :: Int,
    simhlMemoryIn :: [[ValueType]],
    simhlMemoryIndex :: Int,           -- For error messages.
    simhlMemoryOut :: [[ValueType]]
}

-- Implementation function for high level simulator.
-- There's some amount of state that needs to be taken care of by the
-- SimhlState data type above.
--   simhlTypesMatch : Bool result of simhlCheckInputs, should be True.
--     So from this point on, we can assume all inputs' types match the
--     input ports of the op being simulated (But memory inputs not checked).
--   simhlUnderutil : Underutilization denominator at the point of the
--     AST being simulated.
--   simhlMemoryIn / simhlMemoryOut : List of "tapes" of inputs/outputs
--     for MemRead / MemWrite. As mentioned above the input order corresponds
--     to the order that the MemReads would be visited by DFS. Everything
--     here is recursively implemented, so to support this ordering, each
--     time a MemRead is encountered, remove the head of simhlMemoryIn, and
--     each time a MemWrite is encountered, append to simhlMemoryOut.
--
-- Output is a tuple of [[ValueType]] and SimhlState, which is how the memory
-- state changes explained above are carried on through the recursion.
simhl :: Op -> [[ValueType]] -> SimhlState -> ([[ValueType]], SimhlState)
simhl op inputs (SimhlState False _ _ _ _) =
    error "Aetherling internal error: simhl function got False Bool value"
    -- If simhlCheckInputs were false, should have gotten proper error message.
simhl (Add t) inputs state = (simhlCombinational simhlAdd inputs, state)
simhl (Sub t) inputs state = (simhlCombinational simhlSub inputs, state)
simhl (Mul t) inputs state = (simhlCombinational simhlMul inputs, state)
simhl (Div t) inputs state = (simhlCombinational simhlDiv inputs, state)
simhl (Max t) inputs state = (simhlCombinational simhlMax inputs, state)
simhl (Min t) inputs state = (simhlCombinational simhlMin inputs, state)
simhl (Ashr c t) inputs state = (simhlCombinational (simhlAshr c) inputs, state)
simhl (Shl c t) inputs state = (simhlCombinational (simhlShl c) inputs, state)
simhl (Abs t) inputs state = (simhlCombinational simhlAbs inputs, state)

simhl (Not t) inputs state = (simhlCombinational simhlNot inputs, state)
simhl (And t) inputs state = (simhlCombinational simhlAnd inputs, state)
simhl (Or t) inputs state = (simhlCombinational simhlOr inputs, state)
simhl (XOr t) inputs state = (simhlCombinational simhlXOr inputs, state)

simhl Eq inputs state = (simhlCombinational simhlEq inputs, state)
simhl Neq inputs state = (simhlCombinational simhlNeq inputs, state)
simhl Lt inputs state = (simhlCombinational simhlLt inputs, state)
simhl Leq inputs state = (simhlCombinational simhlLeq inputs, state)
simhl Gt inputs state = (simhlCombinational simhlGt inputs, state)
simhl Geq inputs state = (simhlCombinational simhlGeq inputs, state)

simhl (Constant_Int a) inputs state =
    (simhlCombinational (simhlInt a) inputs, state)
simhl (Constant_Bit a) inputs state =
    (simhlCombinational (simhlBit a) inputs, state)

simhl (MemRead t) inputs state = simhlRead t inputs state

simhl (MemWrite t) inputs state = simhlWrite inputs state

simhl (DuplicateOutputs 0 op) inputs state = ([[] | input <- inputs], state)
simhl (DuplicateOutputs 1 op) inputs state = simhl op inputs state
simhl (DuplicateOutputs count op) inputs state =
    do { let (outputs, nextState) = simhl op inputs state
       ; ([
             concat $ replicate count output
             | output <- outputs
          ], nextState)
    }

simhl (MapOp par op) inputs state = simhlMap par op inputs state

simhl (Underutil uDenom op) inputs (SimhlState ok sDenom memIn memIdx memOut) =
    simhl op inputs (SimhlState ok (uDenom * sDenom) memIn memIdx memOut)

-- We have to multiply the register delay by the underutil denominator to get
-- the actual delay in cycles (underutil is like a clock enable, if we have
-- a RegDelay N and an underutil of M, then it's like we have N registers whose
-- clock enable is high only once every M cycles).
simhl (RegDelay delay op) inputs state =
    do { let (rawOutputs, nextState) = simhl op inputs state
       ; (simhlDelay (delay * (simhlUnderutil state)) rawOutputs, nextState)
    }

simhl (ComposeSeq []) inputs state = error "ComposeSeq with empty [Op]"
simhl (ComposeSeq [op]) inputs state = simhl op inputs state
simhl (ComposeSeq (op:ops)) inputs state =
    do { let (nextInput, nextState) = simhl op inputs state
       ; simhl (ComposeSeq ops) nextInput nextState
    }

simhl (ComposePar []) inputs state = ([[] | input <- inputs], state)
simhl (ComposePar (op:moreOps)) inputs state =
    do { let (thisOpOutput, nextState) = simhl op
              [fst $ splitAt (length $ inPorts op) i | i <- inputs] -- X
              state
       ; let (moreOpsOutputs, endState) = simhl (ComposePar moreOps)
              [snd $ splitAt (length $ inPorts op) i | i <- inputs] -- Y
              nextState
       ; (
           [   -- Glue the port outputs of each clock cycle together.
               opCycleOutput ++ moreOpsCycleOutput
               | (opCycleOutput, moreOpsCycleOutput) <-
               zip thisOpOutput moreOpsOutputs
           ], endState
         )
        -- X is the [[ValueType]] inputs through time that (op) expects, and
        -- Y is the list of remaining inputs unused by X.
    }

simhl (ComposeFailure foo bar) _ _ =
    error $ "Cannot simulate ComposeFaliure " ++ show (ComposeFailure foo bar)
    

-- Helper function for simulating combinational devices.  Takes an
-- implementation function ([ValueType]->[ValueType]) and a list of
-- lists of ValueType as earlier. Implementation function takes a list
-- with entries corresponding to input ports' inputs in 1 cycle and
-- produces list of output ports' outputs.
simhlCombinational :: ([ValueType]->[ValueType]) -> [[ValueType]] -> [[ValueType]]
simhlCombinational impl [] = []
simhlCombinational impl (input:inputs) =
    (impl input):(simhlCombinational impl inputs)

-- Given implementations for Ints and Bools, create a [ValueType] -> [ValueType]
-- function suitable for simhlCombinational.
simhlBinaryOp :: (Int -> Int -> Int) -> (Bool -> Bool -> Bool)
                  -> [ValueType] -> [ValueType]
simhlBinaryOp intImpl bitImpl [V_Unit, _] = [V_Unit]
simhlBinaryOp intImpl bitImpl [_, V_Unit] = [V_Unit]
simhlBinaryOp intImpl bitImpl [V_Int x, V_Int y] = [V_Int $ intImpl x y]
simhlBinaryOp intImpl bitImpl [V_Bit x, V_Bit y] = [V_Bit $ bitImpl x y]
simhlBinaryOp intImpl bitImpl [V_Array xs, V_Array ys] =
    [V_Array $ concat [simhlBinaryOp intImpl bitImpl [x, y]
    | (x, y) <- zip xs ys]]
simhlBinaryOp _ _ _ = error "Aetherling internal error: binary op no match"

-- Similar function for unary operators.
simhlUnaryOp :: (Int -> Int) -> (Bool -> Bool)
                -> [ValueType] -> [ValueType]
simhlUnaryOp intImpl bitImpl [V_Unit] = [V_Unit]
simhlUnaryOp intImpl bitImpl [V_Int x] = [V_Int $ intImpl x]
simhlUnaryOp intImpl bitImpl [V_Bit x] = [V_Bit $ bitImpl x]
simhlUnaryOp intImpl bitImpl [V_Array xs] =
    [V_Array $ concat [simhlUnaryOp intImpl bitImpl [x] | x <- xs]]
simhlUnaryOp _ _ _ = error "Aetherling internal error: unary op no match"

-- Similar function for int comparison operators (int + int -> bool).
simhlIntCmpOp :: (Int -> Int -> Bool) -> [ValueType] -> [ValueType]
simhlIntCmpOp intImpl [V_Unit, _] = [V_Unit]
simhlIntCmpOp intImpl [_, V_Unit] = [V_Unit]
simhlIntCmpOp intImpl [V_Int x, V_Int y] = [V_Bit $ intImpl x y]
simhlIntCmpOp _ _ = error "Aetherling internal error: int cmp op no match"

-- Combinational device implementations.
simhlAdd :: [ValueType] -> [ValueType]
simhlAdd = simhlBinaryOp (\x y -> x+y) xor

simhlSub :: [ValueType] -> [ValueType]
simhlSub = simhlBinaryOp (\x y -> x-y) xor

simhlMul :: [ValueType] -> [ValueType]
simhlMul = simhlBinaryOp (\x y -> x*y) (\x y -> x && y)

simhlDiv :: [ValueType] -> [ValueType]
simhlDiv = simhlBinaryOp div (\x y -> x && y) -- XXX bit division???

simhlMax :: [ValueType] -> [ValueType]
simhlMax = simhlBinaryOp max max

simhlMin :: [ValueType] -> [ValueType]
simhlMin = simhlBinaryOp min min

simhlAshr :: Int -> [ValueType] -> [ValueType]
simhlAshr c = simhlUnaryOp (\x -> div x (2^c)) (\x -> x) -- XXX bit shift???

simhlShl :: Int -> [ValueType] -> [ValueType]
simhlShl c = simhlUnaryOp (\x -> x * (2^c)) (\x -> x && (c /= 0))

simhlAbs :: [ValueType] -> [ValueType]
simhlAbs = simhlUnaryOp abs (\x -> x)

simhlNot :: [ValueType] -> [ValueType]
simhlNot = simhlUnaryOp complement not

simhlAnd :: [ValueType] -> [ValueType]
simhlAnd = simhlBinaryOp (.&.) (&&)

simhlOr :: [ValueType] -> [ValueType]
simhlOr = simhlBinaryOp (.|.) (||)

simhlXOr :: [ValueType] -> [ValueType]
simhlXOr = simhlBinaryOp xor xor

simhlEq :: [ValueType] -> [ValueType]
simhlEq = simhlIntCmpOp (==)

simhlNeq :: [ValueType] -> [ValueType]
simhlNeq = simhlIntCmpOp (/=)

simhlLt :: [ValueType] -> [ValueType]
simhlLt = simhlIntCmpOp (<)

simhlLeq :: [ValueType] -> [ValueType]
simhlLeq = simhlIntCmpOp (<=)

simhlGt :: [ValueType] -> [ValueType]
simhlGt = simhlIntCmpOp (>)

simhlGeq :: [ValueType] -> [ValueType]
simhlGeq = simhlIntCmpOp (>=)

simhlInt :: [Int] -> [ValueType] -> [ValueType]
simhlInt ints _ = [V_Array [V_Int i | i <- ints]]

simhlBit :: [Bool] -> [ValueType] -> [ValueType]
simhlBit bools _ = [V_Array [V_Bit b | b <- bools]]

-- Memory read and write implementations.
-- Three things need to happen to the state object:
--   * Take into account the underutil denominator d. Only read or write
--     on clock cycles N where (N = d-1) mod d.
--   * Either remove the first tape entry from memIn or append a tape
--     of output to memOut. See DFS comment.
--   * If reading, update the memIdx. This is for error reporting.
simhlRead :: TokenType -> [[ValueType]] -> SimhlState
          -> ([[ValueType]], SimhlState)
simhlRead _ _ (SimhlState _ _ [] memIdx _) =
    error("Ran out of memory input at MemRead number "
           ++ show memIdx
           ++ " (numbered using DFS starting at 0)."
    )
simhlRead t inputs (SimhlState ok denom (inTape:inTapes) memIdx memOut) =
    -- Look at the inputs (should just be a list of empty lists). Its
    -- length tells us how many outputs we should create. Then just
    -- peel off the first, pad with (denom-1) V_Units before each
    -- item, doing type checking and padding with extra V_Units if we
    -- run out.
    if all (\v -> v == V_Unit || vtTypesMatch v t) inTape then
    (
        concat [
            (replicate (denom-1) [V_Unit]) ++ [[tapeEntry]]
            | tapeEntry
            <- fst $ splitAt (length inputs) (inTape ++ repeat V_Unit)
        ],
        SimhlState ok denom inTapes memIdx memOut
    )
    else error("At MemRead number " ++ show memIdx
            ++ " (numbered using DFS, starting from 0), input "
            ++ show inTape
            ++ " does not match expected type "
            ++ show t)

simhlWrite :: [[ValueType]] -> SimhlState
           -> ( [[ValueType]], SimhlState )
simhlWrite inputs (SimhlState ok denom memIn memIdx memOut) =
    -- Take every denom'th input and write it to a new memOut entry in state.
    (
        replicate (length inputs) [],
        SimhlState
          ok
          denom
          memIn
          memIdx
          (memOut ++ [[
            v | ([v], idx) <- filter (\ (_, i) -> i `mod` denom == i-1)
                              (zip inputs [1,2..])
          ]])
    )

-- Split the inputs to a MapOp into the inputs expected by each Op
-- contained within the MapOp. If N is the parallelism of the MapOp,
-- we expect the input to be a list of lists of V_Arrays of length N,
-- and the output will be a list of N (lists of lists of ValueType).
-- par argument is parallelism, used for checking.
simhlSplitMapInputs :: Int -> [[ValueType]] -> [[[ValueType]]]
simhlSplitMapInputs 0 inputs =
    if all (\i -> all (\v -> v == V_Unit || v == V_Array []) i) inputs then []
    else error "Aetherling internal error: leftover arguments for map"
simhlSplitMapInputs par inputs =
    [[valueTypeHead arg | arg <- args] | args <- inputs]
    :simhlSplitMapInputs (par-1)
    [[valueTypeTail arg | arg <- args] | args <- inputs]

-- Reverse operation of simhlSplitMapInputs
simhlJoinMapOutputs ::  Int -> [[[ValueType]]] -> [[ValueType]]
simhlJoinMapOutputs 0 [] = []
simhlJoinMapOutputs 1 [output] =
    [
        [
            V_Array [thing] | thing <- seq
        ]
        | seq <- output
    ]
simhlJoinMapOutputs par (output:outputs) =
    [
        [
            V_Array (thing:things)
            | (thing, V_Array things) <- (zip seq seqs)
        ]
        | (seq, seqs) <- zip output (simhlJoinMapOutputs (par-1) outputs)
    ]
simhlJoinMapOutputs _ _ = error "Aetherling internal error: broken map join."

-- Fold strategy for Map:
-- We need to get one set of inputs in and one set of outputs to each Op
-- in the map. However, the state must go through each Op in sequence
-- (to preserve the DFS order of the memory state in the State object).
-- So, the tuple has a [[[ValueType]]] that collects all the outputs of
-- each op and a SimhlState that's passed through each op. The laneInput
-- is the "slice" of the input array corresponding to this Op.
simhlMapFoldLambda :: (Op, [[[ValueType]]], SimhlState)
                   -> [[ValueType]]
                   -> (Op, [[[ValueType]]], SimhlState)
simhlMapFoldLambda lastTuple laneInput =
    do { let (theMappedOp, lastOutputs, lastState) = lastTuple
       ; let (thisOpOutputs, nextState) = simhl theMappedOp laneInput lastState
       ; (theMappedOp, lastOutputs ++ [thisOpOutputs], nextState)
    }

-- Glue together the above 3 things to get the map operator simulated.
simhlMap :: Int -> Op -> [[ValueType]] -> SimhlState
         -> ( [[ValueType]], SimhlState )
simhlMap par theMappedOp inputs state =
    do { let (_, mapOutputs, endState) = foldl simhlMapFoldLambda
                                               (theMappedOp, [], state)
                                               (simhlSplitMapInputs par inputs)
       ; (simhlJoinMapOutputs par mapOutputs, endState)
    }

-- Delay the input [[ValueType]] (in simulateHighLevel port input format) by
-- N cycles. Do this by discarding the last N entries and prepending N entries
-- of V_Unit lists.
simhlDelay :: Int -> [[ValueType]] -> [[ValueType]]
simhlDelay _ [] = []
simhlDelay 0 inputs = inputs
simhlDelay n inputs  =
    if n < 0 then error "Negative register delay." else
        simhlDelay (n-1)  ([V_Unit | x <- last inputs]:(init inputs))
