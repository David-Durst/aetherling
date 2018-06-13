module SpaceTime.STExamples where
import SpaceTime.STTypes
import SpaceTime.STMetrics
import SpaceTime.STOpClasses
import SpaceTime.STOps

exMemRead = Just (fullIterSF 1 1 $ SFLeafOp $ MemRead 10 $ T_Int)
exPart = Just (ComposeContainer $ IterOp 1 1 $ SFLeafOp $ 
  StreamArrayController (1, T_Int) (1, T_Array 1 T_Int))
exLB = Just (ComposeContainer $ IterOp 1 1 $ SFLeafOp $ LineBuffer 1 3 T_Int)
exFlat = Just (ComposeContainer $ IterOp 1 1 $ SFLeafOp $ 
  StreamArrayController (1, T_Array 1 (T_Array 3 T_Int)) (1, T_Array 3 T_Int))
exConst = Just (ComposeContainer $ RegDelay 3 0 $ IterOp 13 10 $
    SFLeafOp $ Constant_Int 1 [1, 1, 1])

conv1PxPerClock = 
  (
    (
      exMemRead |>>=|
      fmap (ComposeContainer . IterOp 10 10) (
        exPart |>>=|
        exLB |>>=|
        exFlat
      )
    ) |&|
    exConst
  ) |>>=|
  fmap (ComposeContainer . IterOp 10 10) (
    Just (ComposeContainer $ IterOp 1 1 $ MapOp 3 3 $ SFLeafOp $ Add T_Int) |>>=|
    Just (ComposeContainer $ IterOp 1 1 $ ReduceOp 3 3 $ SFLeafOp $ Add T_Int)
  ) |>>=|
  Just (fullIterSF 1 1 $ SFLeafOp $ MemWrite 10 $ T_Int)
