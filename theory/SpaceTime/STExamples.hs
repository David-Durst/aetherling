module SpaceTime.STExamples where
import SpaceTime.STTypes
import SpaceTime.STMetrics
import SpaceTime.STOpClasses
import SpaceTime.STOps

exMemRead = Just (ComposeContainer $ MemRead 10 $ T_Int)
exPart = Just (ComposeContainer $
  StreamArrayController (1, T_Int) (1, T_Array 1 T_Int))
exLB = Just (ComposeContainer $ LineBuffer 1 3 T_Int)
exFlat = Just (ComposeContainer $ 
  StreamArrayController (1, T_Array 1 (T_Array 3 T_Int)) (1, T_Array 3 T_Int))
exConst = Just (ComposeContainer $ RegDelay 3 0 $ IterOp 13 10 $ Constant_Int 1 [1, 1, 1])

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
    Just (ComposeContainer $ MapOp 3 3 $ Add T_Int) |>>=|
    Just (ComposeContainer $ ReduceOp 3 3 $ Add T_Int)
  ) |>>=|
  Just (ComposeContainer $ MemWrite 10 $ T_Int)
