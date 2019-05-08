from aetherling.modules.reduce import DefineReduceSequential, DefineReduceParallel, renameCircuitForReduce
from aetherling.modules.register_any_type import DefineRegisterAnyType
from aetherling.modules.term_any_type import TermAnyType
from aetherling.modules.noop import DefineNoop
from magma.backend.coreir_ import CoreIRBackend
from magma.bitutils import *
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from mantle.coreir.arith import *
from mantle.coreir.logic import *
from mantle.coreir.compare import *
from mantle.coreir import DefineCoreirConst
from mantle.coreir.LUT import *
from aetherling.modules.upsample import *
from aetherling.modules.downsample import *
from aetherling.modules.reduce import *
from aetherling.modules.native_linebuffer.two_dimensional_native_linebuffer import DefineTwoDimensionalLineBuffer

c = coreir.Context()
cirb = CoreIRBackend(c)

args = ['I0', Array[8, In(Bit)], 'I1', Array[8, In(Bit)], 'I2', Array[8, In(Bit)], 'I3', Array[8, In(Bit)], 'I4', Array[8, In(Bit)], 'I5', Array[8, In(Bit)], 'I6', Array[8, In(Bit)], 'I7', Array[8, In(Bit)], 'I8', Array[8, In(Bit)], 'I9', Array[8, In(Bit)], 'I10', Array[8, In(Bit)], 'I11', Array[8, In(Bit)], 'I12', Array[8, In(Bit)], 'I13', Array[8, In(Bit)], 'I14', Array[8, In(Bit)], 'I15', Array[8, In(Bit)], 'O0', Array[8, Out(Bit)], 'valid_data_in', In(Bit), 'ready_data_in', Out(Bit), 'valid_data_out', Out(Bit), 'ready_data_out', In(Bit), ] + ClockInterface(has_ce=True)
downsample_256x256_to_32x32_16px_in_per_clk = DefineCircuit('downsample_256x256_to_32x32_16px_in_per_clk_Circuit', *args)
magmaInstance0 = DefineTwoDimensionalLineBuffer(cirb, Array[8, In(Bit)], 16, 1, 2, 2, 256, 256, 2, 2, 0, 0)()
magmaInstance1 = DefineNoop(DefineTwoDimensionalLineBuffer(cirb, Array[8, In(Bit)], 16, 1, 2, 2, 256, 256, 2, 2, 0, 0))()
magmaInstance2 = DefineCoreirConst(8, 1)()
magmaInstance3 = DefineCoreirConst(8, 1)()
magmaInstance4 = DefineCoreirConst(8, 1)()
magmaInstance5 = DefineCoreirConst(8, 1)()
magmaInstance6 = DefineCoreirConst(8, 2)()
magmaInstance7 = DefineCoreirConst(8, 2)()
magmaInstance8 = DefineCoreirConst(8, 2)()
magmaInstance9 = DefineCoreirConst(8, 2)()
magmaInstance13 = DefineCoreirConst(8, 3)()
magmaInstance14 = DefineCoreirConst(8, 3)()
magmaInstance15 = DefineCoreirConst(8, 3)()
magmaInstance16 = DefineCoreirConst(8, 3)()
magmaInstance17 = DefineCoreirConst(8, 4)()
magmaInstance18 = DefineCoreirConst(8, 4)()
magmaInstance19 = DefineCoreirConst(8, 4)()
magmaInstance20 = DefineCoreirConst(8, 4)()
wire(magmaInstance0.O[0][0][0], magmaInstance1.in_O[0][0][0])
wire(magmaInstance0.O[0][0][1], magmaInstance1.in_O[0][0][1])
wire(magmaInstance0.O[0][1][0], magmaInstance1.in_O[0][1][0])
wire(magmaInstance0.O[0][1][1], magmaInstance1.in_O[0][1][1])
wire(magmaInstance0.O[1][0][0], magmaInstance1.in_O[1][0][0])
wire(magmaInstance0.O[1][0][1], magmaInstance1.in_O[1][0][1])
wire(magmaInstance0.O[1][1][0], magmaInstance1.in_O[1][1][0])
wire(magmaInstance0.O[1][1][1], magmaInstance1.in_O[1][1][1])
wire(magmaInstance0.O[2][0][0], magmaInstance1.in_O[2][0][0])
wire(magmaInstance0.O[2][0][1], magmaInstance1.in_O[2][0][1])
wire(magmaInstance0.O[2][1][0], magmaInstance1.in_O[2][1][0])
wire(magmaInstance0.O[2][1][1], magmaInstance1.in_O[2][1][1])
wire(magmaInstance0.O[3][0][0], magmaInstance1.in_O[3][0][0])
wire(magmaInstance0.O[3][0][1], magmaInstance1.in_O[3][0][1])
wire(magmaInstance0.O[3][1][0], magmaInstance1.in_O[3][1][0])
wire(magmaInstance0.O[3][1][1], magmaInstance1.in_O[3][1][1])
magmaInstance30 = DefineCoreirMul(8)()
magmaInstance31 = DefineCoreirMul(8)()
magmaInstance32 = DefineCoreirMul(8)()
magmaInstance33 = DefineCoreirMul(8)()
magmaInstance34 = DefineCoreirMul(8)()
magmaInstance35 = DefineCoreirMul(8)()
magmaInstance36 = DefineCoreirMul(8)()
magmaInstance37 = DefineCoreirMul(8)()
magmaInstance38 = DefineCoreirMul(8)()
magmaInstance39 = DefineCoreirMul(8)()
magmaInstance40 = DefineCoreirMul(8)()
magmaInstance41 = DefineCoreirMul(8)()
magmaInstance42 = DefineCoreirMul(8)()
magmaInstance43 = DefineCoreirMul(8)()
magmaInstance44 = DefineCoreirMul(8)()
magmaInstance45 = DefineCoreirMul(8)()
wire(magmaInstance1.O[0][0][0], magmaInstance30.I0)
wire(magmaInstance2.O, magmaInstance30.I1)
wire(magmaInstance1.O[0][0][1], magmaInstance31.I0)
wire(magmaInstance6.O, magmaInstance31.I1)
wire(magmaInstance1.O[0][1][0], magmaInstance32.I0)
wire(magmaInstance13.O, magmaInstance32.I1)
wire(magmaInstance1.O[0][1][1], magmaInstance33.I0)
wire(magmaInstance17.O, magmaInstance33.I1)
wire(magmaInstance1.O[1][0][0], magmaInstance34.I0)
wire(magmaInstance3.O, magmaInstance34.I1)
wire(magmaInstance1.O[1][0][1], magmaInstance35.I0)
wire(magmaInstance7.O, magmaInstance35.I1)
wire(magmaInstance1.O[1][1][0], magmaInstance36.I0)
wire(magmaInstance14.O, magmaInstance36.I1)
wire(magmaInstance1.O[1][1][1], magmaInstance37.I0)
wire(magmaInstance18.O, magmaInstance37.I1)
wire(magmaInstance1.O[2][0][0], magmaInstance38.I0)
wire(magmaInstance4.O, magmaInstance38.I1)
wire(magmaInstance1.O[2][0][1], magmaInstance39.I0)
wire(magmaInstance8.O, magmaInstance39.I1)
wire(magmaInstance1.O[2][1][0], magmaInstance40.I0)
wire(magmaInstance15.O, magmaInstance40.I1)
wire(magmaInstance1.O[2][1][1], magmaInstance41.I0)
wire(magmaInstance19.O, magmaInstance41.I1)
wire(magmaInstance1.O[3][0][0], magmaInstance42.I0)
wire(magmaInstance5.O, magmaInstance42.I1)
wire(magmaInstance1.O[3][0][1], magmaInstance43.I0)
wire(magmaInstance9.O, magmaInstance43.I1)
wire(magmaInstance1.O[3][1][0], magmaInstance44.I0)
wire(magmaInstance16.O, magmaInstance44.I1)
wire(magmaInstance1.O[3][1][1], magmaInstance45.I0)
wire(magmaInstance20.O, magmaInstance45.I1)
magmaInstance46 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance47 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance48 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance49 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance46_identityGen = DefineCoreirConst(8, 0)()
wire(magmaInstance46_identityGen.O, magmaInstance46.I.identity)
wire(magmaInstance46_identityGen.O, magmaInstance47.I.identity)
wire(magmaInstance46_identityGen.O, magmaInstance48.I.identity)
wire(magmaInstance46_identityGen.O, magmaInstance49.I.identity)
wire(magmaInstance30.O, magmaInstance46.I.data[0])
wire(magmaInstance31.O, magmaInstance46.I.data[1])
wire(magmaInstance32.O, magmaInstance46.I.data[2])
wire(magmaInstance33.O, magmaInstance46.I.data[3])
wire(magmaInstance34.O, magmaInstance47.I.data[0])
wire(magmaInstance35.O, magmaInstance47.I.data[1])
wire(magmaInstance36.O, magmaInstance47.I.data[2])
wire(magmaInstance37.O, magmaInstance47.I.data[3])
wire(magmaInstance38.O, magmaInstance48.I.data[0])
wire(magmaInstance39.O, magmaInstance48.I.data[1])
wire(magmaInstance40.O, magmaInstance48.I.data[2])
wire(magmaInstance41.O, magmaInstance48.I.data[3])
wire(magmaInstance42.O, magmaInstance49.I.data[0])
wire(magmaInstance43.O, magmaInstance49.I.data[1])
wire(magmaInstance44.O, magmaInstance49.I.data[2])
wire(magmaInstance45.O, magmaInstance49.I.data[3])
magmaInstance50 = DefineNoop(DefineCoreirConst(8, 0))()
magmaInstance51 = DefineNoop(DefineCoreirConst(8, 0))()
magmaInstance52 = DefineNoop(DefineCoreirConst(8, 0))()
magmaInstance53 = DefineNoop(DefineCoreirConst(8, 0))()
magmaInstance54 = DefineCoreirConst(8, 4)()
magmaInstance55 = DefineCoreirConst(8, 4)()
magmaInstance56 = DefineCoreirConst(8, 4)()
magmaInstance57 = DefineCoreirConst(8, 4)()
wire(magmaInstance46.out, magmaInstance50.in_O)
wire(magmaInstance47.out, magmaInstance51.in_O)
wire(magmaInstance48.out, magmaInstance52.in_O)
wire(magmaInstance49.out, magmaInstance53.in_O)
magmaInstance61 = DefineCoreirUDiv(8)()
magmaInstance62 = DefineCoreirUDiv(8)()
magmaInstance63 = DefineCoreirUDiv(8)()
magmaInstance64 = DefineCoreirUDiv(8)()
wire(magmaInstance50.O, magmaInstance61.I0)
wire(magmaInstance54.O, magmaInstance61.I1)
wire(magmaInstance51.O, magmaInstance62.I0)
wire(magmaInstance55.O, magmaInstance62.I1)
wire(magmaInstance52.O, magmaInstance63.I0)
wire(magmaInstance56.O, magmaInstance63.I1)
wire(magmaInstance53.O, magmaInstance64.I0)
wire(magmaInstance57.O, magmaInstance64.I1)
magmaInstance65 = DefineTwoDimensionalLineBuffer(cirb, Array[8, In(Bit)], 4, 1, 2, 2, 128, 128, 2, 2, 0, 0)()
wire(magmaInstance61.O, magmaInstance65.I[0][0])
wire(magmaInstance62.O, magmaInstance65.I[0][1])
wire(magmaInstance63.O, magmaInstance65.I[0][2])
wire(magmaInstance64.O, magmaInstance65.I[0][3])
magmaInstance66 = DefineNoop(DefineTwoDimensionalLineBuffer(cirb, Array[8, In(Bit)], 4, 1, 2, 2, 128, 128, 2, 2, 0, 0))()
magmaInstance67 = DefineCoreirConst(8, 1)()
magmaInstance68 = DefineCoreirConst(8, 2)()
magmaInstance69 = DefineCoreirConst(8, 3)()
magmaInstance70 = DefineCoreirConst(8, 4)()
wire(magmaInstance65.O[0][0][0], magmaInstance66.in_O[0][0][0])
wire(magmaInstance65.O[0][0][1], magmaInstance66.in_O[0][0][1])
wire(magmaInstance65.O[0][1][0], magmaInstance66.in_O[0][1][0])
wire(magmaInstance65.O[0][1][1], magmaInstance66.in_O[0][1][1])
magmaInstance71 = DefineCoreirMul(8)()
magmaInstance72 = DefineCoreirMul(8)()
magmaInstance73 = DefineCoreirMul(8)()
magmaInstance74 = DefineCoreirMul(8)()
wire(magmaInstance66.O[0][0][0], magmaInstance71.I0)
wire(magmaInstance67.O, magmaInstance71.I1)
wire(magmaInstance66.O[0][0][1], magmaInstance72.I0)
wire(magmaInstance68.O, magmaInstance72.I1)
wire(magmaInstance66.O[0][1][0], magmaInstance73.I0)
wire(magmaInstance69.O, magmaInstance73.I1)
wire(magmaInstance66.O[0][1][1], magmaInstance74.I0)
wire(magmaInstance70.O, magmaInstance74.I1)
magmaInstance75 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance75_identityGen = DefineCoreirConst(8, 0)()
wire(magmaInstance75_identityGen.O, magmaInstance75.I.identity)
wire(magmaInstance71.O, magmaInstance75.I.data[0])
wire(magmaInstance72.O, magmaInstance75.I.data[1])
wire(magmaInstance73.O, magmaInstance75.I.data[2])
wire(magmaInstance74.O, magmaInstance75.I.data[3])
magmaInstance76 = DefineNoop(DefineCoreirConst(8, 0))()
magmaInstance77 = DefineCoreirConst(8, 4)()
wire(magmaInstance75.out, magmaInstance76.in_O)
magmaInstance78 = DefineCoreirUDiv(8)()
wire(magmaInstance76.O, magmaInstance78.I0)
wire(magmaInstance77.O, magmaInstance78.I1)
magmaInstance79 = DefineTwoDimensionalLineBuffer(cirb, Array[8, In(Bit)], 1, 1, 2, 2, 64, 64, 2, 2, 0, 0)()
wire(magmaInstance78.O, magmaInstance79.I[0][0])
magmaInstance80 = DefineNoop(DefineTwoDimensionalLineBuffer(cirb, Array[8, In(Bit)], 1, 1, 2, 2, 64, 64, 2, 2, 0, 0))()
magmaInstance81 = DefineCoreirConst(8, 1)()
magmaInstance82 = DefineCoreirConst(8, 2)()
magmaInstance83 = DefineCoreirConst(8, 3)()
magmaInstance84 = DefineCoreirConst(8, 4)()
wire(magmaInstance79.O[0][0][0], magmaInstance80.in_O[0][0][0])
wire(magmaInstance79.O[0][0][1], magmaInstance80.in_O[0][0][1])
wire(magmaInstance79.O[0][1][0], magmaInstance80.in_O[0][1][0])
wire(magmaInstance79.O[0][1][1], magmaInstance80.in_O[0][1][1])
magmaInstance85 = DefineCoreirMul(8)()
magmaInstance86 = DefineCoreirMul(8)()
magmaInstance87 = DefineCoreirMul(8)()
magmaInstance88 = DefineCoreirMul(8)()
wire(magmaInstance80.O[0][0][0], magmaInstance85.I0)
wire(magmaInstance81.O, magmaInstance85.I1)
wire(magmaInstance80.O[0][0][1], magmaInstance86.I0)
wire(magmaInstance82.O, magmaInstance86.I1)
wire(magmaInstance80.O[0][1][0], magmaInstance87.I0)
wire(magmaInstance83.O, magmaInstance87.I1)
wire(magmaInstance80.O[0][1][1], magmaInstance88.I0)
wire(magmaInstance84.O, magmaInstance88.I1)
magmaInstance89 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance89_identityGen = DefineCoreirConst(8, 0)()
wire(magmaInstance89_identityGen.O, magmaInstance89.I.identity)
wire(magmaInstance85.O, magmaInstance89.I.data[0])
wire(magmaInstance86.O, magmaInstance89.I.data[1])
wire(magmaInstance87.O, magmaInstance89.I.data[2])
wire(magmaInstance88.O, magmaInstance89.I.data[3])
magmaInstance90 = DefineNoop(DefineCoreirConst(8, 0))()
magmaInstance91 = DefineCoreirConst(8, 4)()
wire(magmaInstance89.out, magmaInstance90.in_O)
magmaInstance92 = DefineCoreirUDiv(8)()
wire(magmaInstance90.O, magmaInstance92.I0)
wire(magmaInstance91.O, magmaInstance92.I1)
wire(downsample_256x256_to_32x32_16px_in_per_clk.I0, magmaInstance0.I[0][0])
wire(downsample_256x256_to_32x32_16px_in_per_clk.I1, magmaInstance0.I[0][1])
wire(downsample_256x256_to_32x32_16px_in_per_clk.I2, magmaInstance0.I[0][2])
wire(downsample_256x256_to_32x32_16px_in_per_clk.I3, magmaInstance0.I[0][3])
wire(downsample_256x256_to_32x32_16px_in_per_clk.I4, magmaInstance0.I[0][4])
wire(downsample_256x256_to_32x32_16px_in_per_clk.I5, magmaInstance0.I[0][5])
wire(downsample_256x256_to_32x32_16px_in_per_clk.I6, magmaInstance0.I[0][6])
wire(downsample_256x256_to_32x32_16px_in_per_clk.I7, magmaInstance0.I[0][7])
wire(downsample_256x256_to_32x32_16px_in_per_clk.I8, magmaInstance0.I[0][8])
wire(downsample_256x256_to_32x32_16px_in_per_clk.I9, magmaInstance0.I[0][9])
wire(downsample_256x256_to_32x32_16px_in_per_clk.I10, magmaInstance0.I[0][10])
wire(downsample_256x256_to_32x32_16px_in_per_clk.I11, magmaInstance0.I[0][11])
wire(downsample_256x256_to_32x32_16px_in_per_clk.I12, magmaInstance0.I[0][12])
wire(downsample_256x256_to_32x32_16px_in_per_clk.I13, magmaInstance0.I[0][13])
wire(downsample_256x256_to_32x32_16px_in_per_clk.I14, magmaInstance0.I[0][14])
wire(downsample_256x256_to_32x32_16px_in_per_clk.I15, magmaInstance0.I[0][15])
wire(downsample_256x256_to_32x32_16px_in_per_clk.O0, magmaInstance92.O)
wire(magmaInstance0.ready, downsample_256x256_to_32x32_16px_in_per_clk.ready_data_in)
wire(magmaInstance79.valid, downsample_256x256_to_32x32_16px_in_per_clk.valid_data_out)
wire(downsample_256x256_to_32x32_16px_in_per_clk.valid_data_in & magmaInstance65.ready & bit(downsample_256x256_to_32x32_16px_in_per_clk.CE), magmaInstance0.CE)
wire(magmaInstance0.valid & magmaInstance79.ready & bit(downsample_256x256_to_32x32_16px_in_per_clk.CE), magmaInstance65.CE)
wire(magmaInstance65.valid & downsample_256x256_to_32x32_16px_in_per_clk.ready_data_out & bit(downsample_256x256_to_32x32_16px_in_per_clk.CE), magmaInstance79.CE)
ceTerm = TermAnyType(cirb, Enable)
wire(ceTerm.I, downsample_256x256_to_32x32_16px_in_per_clk.CE)
EndCircuit()
