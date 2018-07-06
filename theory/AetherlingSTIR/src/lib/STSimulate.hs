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
-- Output: Same format as input, with inner lists corresponding to
-- output ports.
--
-- TODO: More convenient error messages?
simulateHighLevel :: Op -> [[ValueType]] -> [[ValueType]]
-- Check that the types match, then delegate to simhl implementation.
simulateHighLevel op inputs = simhl op inputs (simhlCheckInputs 0 op inputs)

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

-- Implementation function for high level simulator.
-- The bool argument is the output of simhlCheckPorts, and should be true.
-- So, from this point we can just assume that all types are correct (however,
-- we should still be prepared to handle "garbage" V_Unit inputs).
simhl :: Op -> [[ValueType]] -> Bool -> [[ValueType]]
simhl op inputs False =
    error "Aetherling internal error: simhl function got False Bool value"
    -- If simhlCheckInputs was false, should have gotten proper error message.
simhl (Add t) inputs True = simhlCombinational simhlAdd inputs
simhl (Sub t) inputs True = simhlCombinational simhlSub inputs
simhl (Mul t) inputs True = simhlCombinational simhlMul inputs
simhl (Div t) inputs True = simhlCombinational simhlDiv inputs
simhl (Max t) inputs True = simhlCombinational simhlMax inputs
simhl (Min t) inputs True = simhlCombinational simhlMin inputs
simhl (Ashr c t) inputs True = simhlCombinational (simhlAshr c) inputs
simhl (Shl c t) inputs True = simhlCombinational (simhlShl c) inputs
simhl (Abs t) inputs True = simhlCombinational simhlAbs inputs

simhl (Not t) inputs True = simhlCombinational simhlNot inputs
simhl (And t) inputs True = simhlCombinational simhlAnd inputs
simhl (Or t) inputs True = simhlCombinational simhlOr inputs
simhl (XOr t) inputs True = simhlCombinational simhlXOr inputs

simhl Eq inputs True = simhlCombinational simhlEq inputs
simhl Neq inputs True = simhlCombinational simhlNeq inputs
simhl Lt inputs True = simhlCombinational simhlLt inputs
simhl Leq inputs True = simhlCombinational simhlLeq inputs
simhl Gt inputs True = simhlCombinational simhlGt inputs
simhl Geq inputs True = simhlCombinational simhlGeq inputs

simhl (Constant_Int a) inputs True = simhlCombinational (simhlInt a) inputs
simhl (Constant_Bit a) inputs True = simhlCombinational (simhlBit a) inputs

simhl (DuplicateOutputs 0 op) inputs True = [[] | input <- inputs]
simhl (DuplicateOutputs 1 op) inputs True = simhl op inputs True
simhl (DuplicateOutputs count op) inputs True =
    [
        concat $ replicate count output
        | output <- (simhl op inputs True)
    ]

simhl (MapOp par op) inputs True =
    simhlJoinMapOutputs par [simhl op laneInputs True
                       | laneInputs <- simhlSplitMapInputs par inputs]

simhl (ComposeSeq []) inputs True = error "ComposeSeq with empty [Op]"
simhl (ComposeSeq [op]) inputs True = simhl op inputs True
simhl (ComposeSeq (op:ops)) inputs True =
    simhl (ComposeSeq ops) (simhl op inputs True) True

simhl (ComposePar []) inputs True = [[] | input <- inputs]
simhl (ComposePar (op:moreOps)) inputs True =
    [
        opCycleOutput ++ moreOpsCycleOutput
        | (opCycleOutput, moreOpsCycleOutput) <-
        zip
        -- [[ValueType]] output through time of op.
        (
            simhl op
                [fst $ splitAt (length $ inPorts op) i | i <- inputs] -- X
                True
        )
        -- [[ValueType]] output through time of parallel compose of moreOps.
        (
            simhl (ComposePar moreOps)
                [snd $ splitAt (length $ inPorts op) i | i <- inputs] -- Y
                True
        )
        -- X is the [[ValueType]] inputs through time that (op) expects, and
        -- Y is the list of remaining inputs unused by X.
    ]

simhl (ComposeFailure a b) inputs True =
    error $ "Cannot simulate ComposeFaliure " ++ show (ComposeFailure a b)
    

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

