module SpaceTime.STExamples where
import SpaceTime.STTypes
import SpaceTime.STMetrics
import SpaceTime.STOpClasses
import SpaceTime.STOps

exMemRead = Just (ComposeContainer $ FixedOp $ UtilOp 0 $ MemRead 10 $ T_Int)
exPart = Just (ComposeContainer $ ScalableOp $ UtilOp 0 $ IterOp 10 $ ComposeContainer $ UtilOp 0 $ 
  SFNonMappable $ StreamArrayController (1, T_Int) (1, T_Array 1 T_Int))
exLB = Just (ComposeContainer $ FixedOp $ UtilOp 0 $ LineBuffer 1 3 10 T_Int)
exFlat = Just (ComposeContainer $ ScalableOp $ UtilOp 2 $ IterOp 8 $ ComposeContainer $ UtilOp 0 $
  SFNonMappable $ StreamArrayController (1, T_Array 1 (T_Array 3 T_Int)) (1, T_Array 3 T_Int))
exConst = Just (ComposeContainer $ ScalableOp $ UtilOp 8 $ IterOp 8 $ ComposeContainer $ UtilOp 4 $
  SFNonMappable $ Constant_Int 1 [1, 1, 1])

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
  Just (ComposeContainer $ ScalableOp $ UtilOp 2 $ IterOp 8 $ ComposeContainer $ 
    UtilOp 0 $ SFHigherOrder $ MapOp 3 3 $ LeafOp $ Add T_Int) |>>=|
  Just (ComposeContainer $ ScalableOp $ UtilOp 2 $ IterOp 8 $ ComposeContainer $ 
    UtilOp 0 $ SFHigherOrder $ ReduceOp 3 3 $ LeafOp $ Add T_Int) |>>=|
  Just (ComposeContainer $ FixedOp $ UtilOp 0 $ MemWrite 8 $ T_Int)
