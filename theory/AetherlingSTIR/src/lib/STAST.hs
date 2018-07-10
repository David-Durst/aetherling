module STAST where
import STTypes
import STMetrics

-- These are leaf nodes that can be used in a higher order operator
data Op =
  -- LEAF OPS
  Add TokenType
  | Sub TokenType
  | Mul TokenType
  | Div TokenType
  | Max TokenType
  | Min TokenType
  | Ashr Int TokenType
  | Shl Int TokenType
  | Abs TokenType
  | Not TokenType
  | And TokenType
  | Or TokenType
  | XOr TokenType
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq
  | MemRead TokenType
  | MemWrite TokenType
  -- first arg is pixels per clock in each dimension. First value in list is outer 
  -- most dimension that iterating over (rows first, columns second in 2d case) 
  -- second arg is window width in each dimension. Same indexing order 
  -- third arg is the size of the image. Saem indexing order. This is necessary
  -- for internal buffer sizing
  -- Last is the type of the pixel element
  | LineBuffer {pxPerClock :: [Int], windowWidth :: [Int], image :: [Int], lbInT :: TokenType}
  -- Array is constant produced, int is sequence length
  | Constant_Int {intConstProduced :: [Int]}
  -- Array is constant produced
  | Constant_Bit {bitConstProduced :: [Bool]}

  -- TYPE MANIPULATORS
  --
  -- Reshapes an input array sequence through space and time.  Buffers
  -- inputs (left-to-right) and emits output only when sufficient
  -- outputs are ready.  Args: input tuple, output tuple, array entry
  -- type. Tuple consists of (sequence length, array length) for the
  -- one input or one output port. Array type may be another array; if
  -- so, it's treated as atomic and not split between two output
  -- cycles.
  | SequenceArrayRepack (Int, Int) (Int, Int) TokenType
  -- First is list of input port types, second is output.
  -- Pure combinational device: decomposes the input and output arrays to
  -- a sequence of wires, and wires up inputs to outputs in order.
  | ArrayReshape [TokenType] [TokenType]
  | DuplicateOutputs Int Op


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

-- SeqPortMismatch indicates couldn't do comopse as composeSeq requires 
-- all port types and latencies 
data ComposeResult = 
  PriorFailure 
  | SeqPortMismatch {outPortsThroughput :: [PortThroughput], inPortsThroughput :: [PortThroughput]}
  | ParLatencyMismash 
  | ComposeSuccess
  deriving (Eq, Show)

-- debugging help methods for parsing syntax tree
-- get the ops contained inside other ops, for going down ComposeFailure trees
getChildOp n op = getChildOps op !! n
getChildOps :: Op -> [Op]
getChildOps (Add t) = []
getChildOps (Sub t) = []
getChildOps (Mul t) = []
getChildOps (Div t) = []
getChildOps (MemRead _) = []
getChildOps (MemWrite _) = []
getChildOps (LineBuffer _ _ _ _) = []
getChildOps (Constant_Int _) = []
getChildOps (Constant_Bit _) = []
getChildOps (SequenceArrayRepack _ _ _) = []
getChildOps (ArrayReshape _ _) = []
getChildOps (MapOp _ op) = [op]
getChildOps (ReduceOp _ _ op) = [op]
getChildOps (Underutil _ op) = [op]
getChildOps (RegDelay _ op) = [op]
getChildOps (ComposePar ops) = ops
getChildOps (ComposeSeq ops) = ops
getChildOps (ComposeFailure _ (op0, op1)) = [op0, op1]

-- Walk the failure tree and find the first one, preferring failures on the left
-- over the right
-- Will return the parent node if not failures
isFailure (ComposeFailure _ _) = True
isFailure _ = False
hasChildWithError op = (<) 0 $ length $ filter (\i -> isFailure i || hasChildWithError i) $ getChildOps op
getFirstError op | hasChildWithError op = head $ map getFirstError $ getChildOps op
getFirstError op | isFailure op = op
getFirstError op = op
