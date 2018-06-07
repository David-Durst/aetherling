module SpaceTime.STExamples where
import SpaceTime.STTypes
import SpaceTime.STMetrics
import SpaceTime.STOpClasses
import SpaceTime.STOps

conv1PxPerClock = 
  Just (ComposeContainerSF (
    Just (ComposeContainerSF (
      Just (ComposeContainerSF [UtilNonMapLeaf 0 (Mem_Read (T_Array 3 T_Int))]) >>=
      Just (ComposeContainerSF [UtilNonMapLeaf 0 (StreamArrayController
        (1, T_Array 3 T_Int) (1, T_Array 1 (T_Array 3 T_Int)))]) >>=
      Just (ComposeContainerSF [UtilNonMapLeaf 0 (LineBuffer (T_Array 3 T_Int))]) >>=
      Just (ComposeContainerSF [UtilNonMapLeaf 0 (StreamArrayController
        (1, T_Array 1 (T_Array 3 T_Int)) (1, T_Array 3 T_Int))]
      ))) |&|
    Just (ComposeContainerSF [UtilNonMapLeaf 0 (Constant_Int 1 [1 1 1])]) 
  )) >>=
  Just (ComposeContainerSF [UtilHigherOrder 0 (MapOp 3 3 (LeafOp $ Add T_Int))]) >>=
  Just (ComposeContainerSF [UtilHigherOrder 0 (ReduceOp 3 3 (LeafOp $ Add T_Int))]) >>=
  Just (ComposeContainerSF [UtilNonMapLeaf 0 (Mem_Write T_Int)])
