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
  -- first Int is pixels per clock, second is window width, third int is 
  | LineBuffer {pxPerClock :: Int, windowWidth :: Int, lbInT :: TokenType}
  -- Array is constant produced
  | Constant_Int {intConstProduced :: [Int]}
  -- Array is constant produced
  | Constant_Bit {bitConstProduced :: [Bool]}

  -- TYPE MANIPULATORS
  -- first pair is input subsequence length and tokens per element, second is output
  | SequenceArrayController [(Int, TokenType)] [(Int, TokenType)]
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
getChildOps (LineBuffer _ _ _) = []
getChildOps (Constant_Int _) = []
getChildOps (Constant_Bit _) = []
getChildOps (SequenceArrayController _ _) = []
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
getFirstError (ComposeFailure PriorFailure (cf@(ComposeFailure _ _), _)) = 
  getFirstError cf
getFirstError (ComposeFailure PriorFailure (_, cf@(ComposeFailure _ _))) = 
  getFirstError cf
getFirstError op = op
