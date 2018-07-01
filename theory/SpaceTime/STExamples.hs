module SpaceTime.STExamples where
import SpaceTime.STTypes
import SpaceTime.STMetrics
import SpaceTime.STAST
import SpaceTime.STAnalysis
import SpaceTime.STComposeOps
import Text.Pretty.Simple (pPrint)

conv1PxPerClock = 
  (
    (
      MemRead T_Int |>>=|
      SequenceArrayController (1, T_Int) (1, T_Array 1 T_Int) |>>=|
      LineBuffer 1 3 T_Int |>>=|
      SequenceArrayController (1, T_Array 1 (T_Array 3 T_Int)) (1, T_Array 3 T_Int)
    ) |&|
    Constant_Int [1, 1, 1]
  ) |>>=|
  MapOp 3 (Add T_Int) |>>=|
  ReduceOp 3 3 (Add T_Int) |>>=|
  MemWrite T_Int

describeMethod name op = do
  print $ "Describing Module: " ++ name
  pPrint op
  print $ "In Ports: " ++ show (inPorts op)
  print $ "Out Ports: " ++ show (outPorts op)
  print $ "Clocks Per Sequence: " ++ show (cps op)
  print $ "Space: " ++ show (space op)
  print $ "Initial Latency: " ++ show (initialLatency op)
  print $ "Maximum Combinational Path: " ++ show (maxCombPath op)
  print $ "Utilization: " ++ show (util op)
  putStr "\n"

main = do
  describeMethod "1 Pixerl Per Clock 3 Pixel Stencil Convolution" conv1PxPerClock
