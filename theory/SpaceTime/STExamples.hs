module SpaceTime.STExamples where
import SpaceTime.STTypes
import SpaceTime.STMetrics
import SpaceTime.STOpClasses
import SpaceTime.STOps

exMemRead = Just (ComposeContainer $ UtilNonMapLeaf 0 $ Mem_Read $ T_Array 3 T_Int)
exPart = Just (ComposeContainer $ UtilNonMapLeaf 0 $ StreamArrayController
  (1, T_Array 3 T_Int) (1, T_Array 1 (T_Array 3 T_Int)))
exLB = Just (ComposeContainer $ UtilNonMapLeaf 0 $ LineBuffer 1 3 (T_Array 3 T_Int))
exFlat = Just (ComposeContainer $ UtilNonMapLeaf 0 $ StreamArrayController
  (1, T_Array 1 (T_Array 3 T_Int)) (1, T_Array 3 T_Int))
exConst = Just (ComposeContainer $ UtilNonMapLeaf 2 $ Constant_Int 1 [1, 1, 1])

conv1PxPerClock = 
  (
    (
      exMemRead |>>=|
      exPart |>>=|
      exLB |>>=|
      exFlat
    ) |&|
    exConst
  ) |>>=|
  Just (ComposeContainer $ UtilHigherOrder 0 $ MapOp 3 3 $ LeafOp $ Add T_Int) |>>=|
  Just (ComposeContainer $ UtilHigherOrder 0 $ ReduceOp 3 3 $ LeafOp $ Add T_Int) |>>=|
  Just (ComposeContainer $ UtilNonMapLeaf 0 $ Mem_Write T_Int)
