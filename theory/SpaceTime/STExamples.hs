module SpaceTime.STExamples where
import SpaceTime.STTypes
import SpaceTime.STMetrics
import SpaceTime.STAST
import SpaceTime.STAnalysis
import SpaceTime.STComposeOps

conv1PxPerClock = 
  (
    (
      MemRead T_Int |>>=|
      SequenceArrayController (1, T_Int) (1, T_Array 1 T_Int) |>>=|
      LineBuffer 1 3 T_Int |>>=|
      SequenceArrayController (1, T_Array 1 (T_Array 3 T_Int)) (1, T_Array 3 T_Int))
    ) |&|
    Constant_Int 1 [1, 1, 1]
  ) |>>=|
  MapOp 3 3 (Add T_Int) |>>=|
  ReduceOp 3 3 (Add T_Int) |>>=|
  MemWrite T_Int

describeMethod name op = do
  print name
  print $ "In Ports: " ++ inPorts op
  print $ "Out Ports: " ++ outPorts op
  print $ "Clocks Per Sequence " ++ cps op
  print $ "Area: " ++ space op
  print $ "Initial Latency: " ++ initialLatency op
  print $ "Maximum Combinational Path: " ++ maxCombPath op
  print $ "Utilization: " ++ util op
  print " "

main = do
  describeMethod "1 Pixerl Per Clock 3 Pixel Stencil Convolution" conv1PxPerClock
