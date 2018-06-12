module SpaceTime.STExamples where
import SpaceTime.STTypes
import SpaceTime.STMetrics
import SpaceTime.STOpClasses
import SpaceTime.STOps

exMemRead = Just (fullUtilSFToIter 1 $ SFLeafOp $ MemRead 10 $ T_Int)
exPart = Just (ComposeContainer $ UtilOp 0 $ SFLeafOp $ 
  StreamArrayController (1, T_Int) (1, T_Array 1 T_Int))
exLB = Just (ComposeContainer $ UtilOp 0 $ SFLeafOp $ LineBuffer 1 3 T_Int)
exFlat = Just (ComposeContainer $ UtilOp 0 $ SFLeafOp $ 
  StreamArrayController (1, T_Array 1 (T_Array 3 T_Int)) (1, T_Array 3 T_Int))
exConst = Just (ComposeContainer $ IterOp 10 $ ComposeContainer $ UtilOp 4 $
  SFLeafOp $ Constant_Int 1 [1, 1, 1])

conv1PxPerClock = 
  (
    (
      exMemRead |>>=|
      fmap (ComposeContainer . IterOp 10) (
        exPart |>>=|
        exLB |>>=|
        exFlat
      )
    ) |&|
    exConst
  ) |>>=|
  fmap (ComposeContainer . IterOp 10) (
    Just (ComposeContainer $ UtilOp 0 $ MapOp 3 3 $ SFLeafOp $ Add T_Int) |>>=|
    Just (ComposeContainer $ UtilOp 0 $ ReduceOp 3 3 $ SFLeafOp $ Add T_Int)
  ) |>>=|
  Just (fullUtilSFToIter 1 $ SFLeafOp $ MemWrite 10 $ T_Int)
