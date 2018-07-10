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
-- list corresponds (in order) to one input port. These inner list
-- entries are a sequence of input values for said port, with each
-- value corresponding to one input on a "meaningful" clock
-- cycle. (i.e.  skip garbage inputs -- for devices that aren't
-- underutilized and have no warmup, this corresponds to a list of
-- inputs on each clock cycle).
--
-- NOTE: Be careful if some ports have a longer/shorter sequence of
-- inputs than expected.
--
-- Memory input: list of lists of ValueType. The inner lists are
-- "tapes" of input corresponding to one MemRead, which outputs the
-- tape's values sequentially. Index i of the outer list corresponds
-- to the input for the ith MemRead, which are numbered in the order
-- they would be visited by a depth-first search (DFS) of the AST.
--
-- NOTE: At time of writing, DFS order may not be well-defined by some
-- ops like ReduceOp.
--
-- Output: Tuple of [[ValueType]], [[ValueType]]. The first is the
-- output of the simulated op's output ports, in the same format as
-- the port inputs.  The second is the output of all MemWrites, in the
-- same format as input memory, and same numbering scheme.
--
-- TODO: More convenient error messages?
-- TODO: Warnings when input sequence lengths don't match.
simulateHighLevel :: Op -> [[ValueType]] -> [[ValueType]]
                  -> ( [[ValueType]], [[ValueType]] )
-- Check that the types match, then delegate to simhl implementation.
simulateHighLevel op portInputs memoryInputs = do {
        let portOutputsAndState = simhl op portInputs
                (SimhlState (simhlCheckInputs 0 (inPorts op) portInputs)
                    memoryInputs 0 []
                )
       ;
       (fst portOutputsAndState, simhlMemoryOut $ snd portOutputsAndState)
}

-- Inspect the inPorts of op and see if they match the sequences of ValueType
-- passed by the user. (Integer argument used to keep track of which
-- list index the error was at).
simhlCheckInputs :: Int -> [PortType] -> [[ValueType]] -> Bool
simhlCheckInputs portIndex [] [] = True
simhlCheckInputs portIndex [] excessValues = error(
    "Have " ++ show portIndex ++ " ports but "
    ++ show (portIndex + (length excessValues))
    ++ " input sequences."
  )
simhlCheckInputs portIndex excessPorts [] = error(
    "Have " ++ show (portIndex + (length excessPorts))
    ++ " ports but only "
    ++ show portIndex ++ " input sequences."
  )
simhlCheckInputs portIndex (portT:portTs) (valSeq:valSeqs) =
    if all (tvTypesMatch (pTType portT)) valSeq
    then simhlCheckInputs (portIndex+1) portTs valSeqs
    else error("At port number " ++ show portIndex ++
               " (counting from 0), input sequence " ++
               show valSeq ++ " does not match port type "
               ++ show (pTType portT)
      )
  
-- Note that this "state" is not the state of the circuit (we don't
-- simulate the circuit in time order, instead, for each Op we calculate
-- all its outputs through time in one step given all inputs through time).
-- This is bookkeeping stuff for type checking and handling memory.
data SimhlState = SimhlState {
    simhlTypesMatch :: Bool,
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
simhl _ _ (SimhlState False _ _ _) =
    error "Aetherling internal error: simhl function got False Bool value"
    -- If simhlCheckInputs were false, should have gotten proper error message.
simhl (Add t) inSeqs state = (simhlCombinational simhlAdd inSeqs, state)
simhl (Sub t) inSeqs state = (simhlCombinational simhlSub inSeqs, state)
simhl (Mul t) inSeqs state = (simhlCombinational simhlMul inSeqs, state)
simhl (Div t) inSeqs state = (simhlCombinational simhlDiv inSeqs, state)
simhl (Max t) inSeqs state = (simhlCombinational simhlMax inSeqs, state)
simhl (Min t) inSeqs state = (simhlCombinational simhlMin inSeqs, state)
simhl (Ashr c t) inSeqs state = (simhlCombinational (simhlAshr c) inSeqs, state)
simhl (Shl c t) inSeqs state = (simhlCombinational (simhlShl c) inSeqs, state)
simhl (Abs t) inSeqs state = (simhlCombinational simhlAbs inSeqs, state)

simhl (Not t) inSeqs state = (simhlCombinational simhlNot inSeqs, state)
simhl (And t) inSeqs state = (simhlCombinational simhlAnd inSeqs, state)
simhl (Or t) inSeqs state = (simhlCombinational simhlOr inSeqs, state)
simhl (XOr t) inSeqs state = (simhlCombinational simhlXOr inSeqs, state)

simhl Eq inSeqs state = (simhlCombinational simhlEq inSeqs, state)
simhl Neq inSeqs state = (simhlCombinational simhlNeq inSeqs, state)
simhl Lt inSeqs state = (simhlCombinational simhlLt inSeqs, state)
simhl Leq inSeqs state = (simhlCombinational simhlLeq inSeqs, state)
simhl Gt inSeqs state = (simhlCombinational simhlGt inSeqs, state)
simhl Geq inSeqs state = (simhlCombinational simhlGeq inSeqs, state)

simhl (Constant_Int a) inSeqs state =
    (simhlCombinational (simhlInt a) inSeqs, state)
simhl (Constant_Bit a) inSeqs state =
    (simhlCombinational (simhlBit a) inSeqs, state)    

simhl (MemRead t) inSeqs state = simhlRead t inSeqs state

simhl (MemWrite t) inSeqs state = simhlWrite inSeqs state

simhl (DuplicateOutputs n op) inSeqs state =
    (concat $ replicate n inSeqs, state)

simhl (MapOp par op) inSeqs state = simhlMap par op inSeqs state
simhl (ReduceOp par numComb op) inSeqs state =
    simhlReduce par numComb op inSeqs state

-- We only care about meaningful inputs and outputs.  Therefore,
-- underutil and register delays should be no-ops in this high level
-- simulator -- dealing with the details of clock scheduling and clock
-- enable is something that we worry about elsewhere.
simhl (Underutil n op) inSeqs state = (inSeqs, state)
simhl (RegDelay delay op) inSeqs state = (inSeqs, state)

simhl (ComposeSeq []) inSeqs state = error "ComposeSeq with empty [Op]"
simhl (ComposeSeq [op]) inSeqs state = simhl op inSeqs state
simhl (ComposeSeq (op:ops)) inSeqs inState =
    do { let (nextInput, nextState) = simhl op inSeqs inState
       ; simhl (ComposeSeq ops) nextInput nextState
    }

simhl (ComposePar []) inSeqs state = ([], state)
simhl (ComposePar (op:moreOps)) inSeqs inState =
    do { let (opInSeqs, moreInSeqs) = splitAt (length $ inPorts op) inSeqs
       ; let (opOutSeqs, nextState) = simhl op opInSeqs inState
       ; let (moreOutSeqs, endState) =
               simhl (ComposePar moreOps) moreInSeqs nextState
       ; (opOutSeqs ++ moreOutSeqs, endState)
    }

simhl (ComposeFailure foo bar) _ _ =
    error $ "Cannot simulate ComposeFaliure " ++ show (ComposeFailure foo bar)

-- Helper function for simulating combinational devices.  Takes an
-- implementation function ([ValueType]->[ValueType]) and a list of
-- lists of ValueType as earlier. Implementation function takes a list
-- with entries corresponding to input ports' inputs in 1 cycle and
-- produces list of output ports' outputs.
simhlCombinational :: ([ValueType]->[ValueType]) -> [[ValueType]] -> [[ValueType]]
simhlCombinational impl inSeqs =
    if any null inSeqs -- inefficient??? Note that we silently ignore
    then []            -- sequence length mismatches for now.
    else do { let inputsNow = map head inSeqs
            ; let inputsL8r = map tail inSeqs
            ; let outputsNow = impl inputsNow
            ; let outputsL8r = simhlCombinational impl inputsL8r
            ; if null outputsL8r
              then [[outputNow] | outputNow <- outputsNow] -- 1-seq output case
              else                                         -- N-seq output case
              [outputNow:outputL8r
              |(outputNow, outputL8r) <- zip outputsNow outputsL8r]
            }

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
-- Two things need to happen to the state object:
--   * Either remove the first tape entry from memIn or append a tape
--     of output to memOut, and return the modified state. See DFS comment.
--   * If reading, update the memIdx. This is for error reporting.
simhlRead :: TokenType -> [[ValueType]] -> SimhlState
          -> ([[ValueType]], SimhlState)
simhlRead _ _ (SimhlState _ [] memIdx _) =
    error("Memory argument too short -- no tape left at MemRead number "
           ++ show memIdx
           ++ " (numbered using DFS starting at 0)."
    )
simhlRead t inputs (SimhlState ok (inTape:inTapes) memIdx memOut) =
    if all (tvTypesMatch t) inTape
    then ([inTape], (SimhlState ok inTapes (memIdx+1) memOut))
    else error("At MemRead number " ++ show memIdx
            ++ " (numbered using DFS, starting from 0), input "
            ++ show inTape
            ++ " does not match expected type "
            ++ show t)
  
simhlWrite :: [[ValueType]] -> SimhlState
           -> ( [[ValueType]], SimhlState )
simhlWrite inputs (SimhlState ok memIn memIdx memOut) =
    ( [], (SimhlState ok memIn memIdx (memOut ++ inputs)) )

-- Implementation of simhl MapOp

-- Helper function for splitting map inputs.
-- The Int is the paralellism.
-- Input should be a list of lists of V_Array, ordered by time then
-- by port as in simulateHighLevel.
-- Output will be list/list/list. The inner 2 correspond to the usual
-- meaning of [[ValueType]]. The outermost list corresponds to the
-- "lanes" of the map operation. In other words, output[i] corresponds
-- to the [[ValueType]] input fed to the ith mapped device.
simhlSplitMapInputs :: Int -> [[ValueType]] -> [[[ValueType]]]
simhlSplitMapInputs 0 _ = []
simhlSplitMapInputs par inSeqs =
    if any null inSeqs
    then error "Aetherling internal error: broken map split."
    else
    [ [valueTypeHead vArray | vArray <- portInSeq]
    | portInSeq <- inSeqs
    ]:
    simhlSplitMapInputs (par-1)
    [ [valueTypeTail vArray | vArray <- portInSeq]
    | portInSeq <- inSeqs
    ]

-- Inverse operation of simhlSplitMapInputs.
simhlJoinMapOutputs :: Int -> [[[ValueType]]] -> [[ValueType]]
simhlJoinMapOutputs 0 _ = []
simhlJoinMapOutputs _ [] = error "Aetherling internal error: broken map join."
simhlJoinMapOutputs par (thisLane:rightLanes) =
    do { let rightJoined = simhlJoinMapOutputs (par-1) rightLanes
       ; [ [V_Array(nowLaneOut:nowLanesOut)
           |(nowLaneOut, V_Array nowLanesOut) <- zip laneOut lanesOut
           ]
         |(laneOut, lanesOut) <- zip thisLane rightJoined
         ]
    }

-- Fold strategy for Map: We need to get one set of inputs in and one
-- set of outputs to each Op in the map. However, the state must go
-- through each Op in sequence (to preserve the DFS order of the
-- memory state in the State object).  So, the tuple has a
-- [[[ValueType]]] that collects all the outputs of each op and a
-- SimhlState that's passed through each op. The laneInput is the
-- "slice" of the input array corresponding to this Op's inputs
-- through time.
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
simhlMap par theMappedOp inSeqs inState =
    do { let (_, mapOutputs, endState) = foldl simhlMapFoldLambda
                                               (theMappedOp, [], inState)
                                               (simhlSplitMapInputs par inSeqs)
       ; (simhlJoinMapOutputs par mapOutputs, endState)
    }


-- Implementation of simhl ReduceOp
-- A Reduce circuit has 2 parts generally.
--   1. A tree of reducedOps that takes par (paralellism) inputs and
--   makes one output.
--   2. A register whose input is the output of a reducedOp, which itself
--   takes the output of said register and the tree as inputs (this part
--   can be omitted if numComb == par, i.e. the reduce is combinational.
--   (assuming combinational reducedOp).
-- After (numComb/par) cycles, the reg's input contains the result of reducing
-- numComb inputs. The reg should be cleared for the next set of numComb inputs.

-- Part 1: The tree of reducedOps. Takes a [[[ValueType]]] input as
-- returned by simhlSplitMapInputs (recycling it for reduce). Since a
-- ReduceOp is defined to take exactly one par-array as input (which
-- is split into par lanes by simhlSplitMapInputs), we expect this
-- [[[ValueType]]] input to have
--
-- > outer list length == par, for the par lanes of input.
-- > middle list length == 1, since there was only one input port.
-- > inner list as a sequence of meaningful inputs over time.
--
-- NOTE: We seem to have a lot of faith that the above is accurate;
-- should we do more checking in case we screwed up?
--
-- Output is a sequence of the tree's output through time (plus new
-- simulator state).  It's just a plain list of ValueType instead of
-- list-of-list since there's only one (imaginary) output port of this
-- tree device.
simhlReduceTree :: Op -> [[[ValueType]]] -> SimhlState
                -> ( [ValueType], SimhlState )
simhlReduceTree theReducedOp laneInSeqs inState =
    do { let ([[treeOutputs]], outState) =
               simhlReduceRecurse theReducedOp laneInSeqs inState
       ; (treeOutputs, outState)
    }

-- Each level of recursion corresponds to one level of the the ecircuit,
-- in which the number of inputs in halved.
simhlReduceRecurse :: Op -> [[[ValueType]]] -> SimhlState
                   -> ( [[[ValueType]]], SimhlState )
simhlReduceRecurse theReducedOp [] state =
    error "Aetherling internal error: 0-input reduce in simulator."
simhlReduceRecurse theReducedOp [oneInput] state = ([oneInput], state)
simhlReduceRecurse theReducedOp splitInputs inState =
    do { let (halfInputs, halfState) =
               simhlReduceTreeLevel theReducedOp splitInputs inState
       ; simhlReduceRecurse theReducedOp halfInputs halfState
    }

simhlReduceTreeLevel :: Op -> [[[ValueType]]] -> SimhlState
                     -> ( [[[ValueType]]], SimhlState )
simhlReduceTreeLevel theReducedOp [] state = ([], state)
simhlReduceTreeLevel theReducedOp [oneInput] state = ([oneInput], state)
simhlReduceTreeLevel theReducedOp ([inSeq0]:[inSeq1]:moreInSeqs) inState =
    do { let twoInSeqs = [inSeq0, inSeq1]
       ; let (oneOutSeq, nextState) = simhl theReducedOp twoInSeqs inState
       ; let (outputsBeyond, outState) =
               simhlReduceTreeLevel theReducedOp moreInSeqs inState
       ; (oneOutSeq:outputsBeyond, outState)
    }
simhlReduceTreeLevel _ _ _ = error "Aethering internal error: something happened."

simhlReduce :: Int -> Int -> Op -> [[ValueType]] -> SimhlState
            -> ( [[ValueType]], SimhlState )
simhlReduce par numComb theReducedOp inSeqs inState =
    do { let laneInSeqs = simhlSplitMapInputs par inSeqs
       ; let (treeOutSeq, outState)
               = simhlReduceTree theReducedOp laneInSeqs inState
       ; if par == numComb
         then ([treeOutSeq], outState) -- Part 2 device unused.
         else ([simhlReduceReg par numComb theReducedOp treeOutSeq], outState)
         -- We have to put the output sequence in a 1-list for the 1
         -- output port of ReduceOp.
    }

-- Function that sorta simulates the register/op cycle (part 2) of the
-- ReduceOp device. Takes a sequence of tree outputs and produces the
-- sequence of outputs that would come out the full ReduceOp by
-- reducing each subsequence of N tree outputs to 1 output, where N =
-- numComb/par.
--
-- We have to assume that theReduceOp is combinational here, so take
-- some liberties in calling it over-and-over again and hiding
-- SimhlState from it.
simhlReduceReg :: Int -> Int -> Op -> [ValueType] -> [ValueType]
simhlReduceReg _ _ _ [] = []
simhlReduceReg par numComb theReducedOp treeOutSeq =
    if par == 0 || numComb == 0 || numComb `mod` par /= 0
    then error "Aetherling internal error: check reduce par/numComb."
    else
      do { let cyclesNeeded = numComb `div` par
         ; let (nowReduce, laterReduce) = splitAt cyclesNeeded treeOutSeq
         ; if length nowReduce < cyclesNeeded
           then []
           else (reduceList nowReduce):
                (simhlReduceReg par numComb theReducedOp laterReduce)
      }
  -- Reduce a subsequence of the tree output sequence (subseq length =
  -- cycles needed per output) into one output. Use foldl since it's
  -- the same order the actual circuit will evaluate the outputs.
  where reduceList valList =
          foldl
          (\x y -> head $ head $ fst $
                   simhl theReducedOp [[x],[y]] (SimhlState True [] 0 [])
          )
          (head valList)
          (tail valList)
             
