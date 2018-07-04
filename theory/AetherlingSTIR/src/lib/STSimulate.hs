{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module STSimulate where
import STTypes
import STMetrics
import STAST
import STAnalysis
import Data.Bool
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
simhl (Add t) inputs True = simhlCombinational (simhlAdd t) inputs
simhl (Sub t) inputs True = simhlCombinational (simhlSub t) inputs
simhl (Mul t) inputs True = simhlCombinational (simhlMul t) inputs
simhl (Div t) inputs True = simhlCombinational (simhlDiv t) inputs
simhl (Constant_Int a) inputs True = simhlCombinational (simhlInt a) inputs
simhl (Constant_Bit a) inputs True = simhlCombinational (simhlBit a) inputs
simhl (MapOp par op) inputs True =
    simhlJoinMapOutputs par [simhl op laneInputs True
                       | laneInputs <- simhlSplitMapInputs par inputs]
simhl (ComposeSeq []) inputs True = error "ComposeSeq with empty [Op]"
simhl (ComposeSeq [op]) inputs True = simhl op inputs True
simhl (ComposeSeq (op:ops)) inputs True =
    simhl (ComposeSeq ops) (simhl op inputs True) True
    

-- Helper function for simulating combinational devices.  Takes an
-- implementation function ([ValueType]->[ValueType]) and a list of
-- lists of ValueType as earlier. Implementation function takes a list
-- with entries corresponding to input ports' inputs in 1 cycle and
-- produces list of output ports' outputs.
simhlCombinational :: ([ValueType]->[ValueType]) -> [[ValueType]] -> [[ValueType]]
simhlCombinational impl [] = []
simhlCombinational impl (input:inputs) =
    (impl input):(simhlCombinational impl inputs)

-- Combinational device implementations.
simhlAdd :: TokenType -> [ValueType] -> [ValueType]
simhlAdd t [V_Unit, _] = [V_Unit]
simhlAdd t [_, V_Unit] = [V_Unit]
simhlAdd T_Int [V_Int x, V_Int y] = [V_Int(x+y)]
simhlAdd T_Bit [V_Bit x, V_Bit y] = [V_Bit(simhlXOR x y)]
simhlAdd (T_Array n t) [V_Array vts0, V_Array vts1] =
    [V_Array $ concat [simhlAdd t [vt0, vt1] | (vt0, vt1) <- zip vts0 vts1]]
simhlAdd t v = error("Atherling internal error: simhlAdd " ++ show t ++ show v)

simhlSub :: TokenType -> [ValueType] -> [ValueType]
simhlSub t [V_Unit, _] = [V_Unit]
simhlSub t [_, V_Unit] = [V_Unit]
simhlSub T_Int [V_Int x, V_Int y] = [V_Int(x-y)]
simhlSub T_Bit [V_Bit x, V_Bit y] = [V_Bit(simhlXOR x y)]
simhlSub (T_Array n t) [V_Array vts0, V_Array vts1] =
    [V_Array $ concat [simhlSub t [vt0, vt1] | (vt0, vt1) <- zip vts0 vts1]]
simhlSub t v = error("Atherling internal error: simhlSub " ++ show t ++ show v)

simhlMul :: TokenType -> [ValueType] -> [ValueType]
simhlMul t [V_Unit, _] = [V_Unit]
simhlMul t [_, V_Unit] = [V_Unit]
simhlMul T_Int [V_Int x, V_Int y] = [V_Int(x*y)]
simhlMul T_Bit [V_Bit x, V_Bit y] = [V_Bit(simhlAND x y)]
simhlMul (T_Array n t) [V_Array vts0, V_Array vts1] =
    [V_Array $ concat [simhlMul t [vt0, vt1] | (vt0, vt1) <- zip vts0 vts1]]
simhlMul t v = error("Atherling internal error: simhlMul " ++ show t ++ show v)

simhlDiv :: TokenType -> [ValueType] -> [ValueType]
simhlDiv t [V_Unit, _] = [V_Unit]
simhlDiv t [_, V_Unit] = [V_Unit]
simhlDiv T_Int [V_Int x, V_Int y] = [V_Int $ div x y] -- XXX div correct???
simhlDiv T_Bit [V_Bit x, V_Bit y] =                   -- XXX bit div?
    error "Aetherling internal error: T_Bit division not implemented."
simhlDiv (T_Array n t) [V_Array vts0, V_Array vts1] =
    [V_Array $ concat [simhlDiv t [vt0, vt1] | (vt0, vt1) <- zip vts0 vts1]]
simhlDiv t v = error("Atherling internal error: simhlDiv " ++ show t ++ show v)

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

-- Doodads
simhlXOR :: Bool -> Bool -> Bool
simhlXOR True x = not x
simhlXOR False x = x

simhlAND :: Bool -> Bool -> Bool
simhlAND True True = True
simhlAND _ _ = False
