module SpaceTime.STAST where
import SpaceTime.STTypes

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

-- SeqPortMismatch indicates couldn't do comopse as composeSeq requires 
-- all port types and latencies 
data ComposeResult = SeqPortMismatch | ParLatencyMismash | ComposeSuccess
  deriving (Eq, Show)
