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

args = ['I0', Array(8, In(Bit)), 'I1', Array(8, In(Bit)), 'I2', Array(8, In(Bit)), 'I3', Array(8, In(Bit)), 'I4', Array(8, In(Bit)), 'I5', Array(8, In(Bit)), 'I6', Array(8, In(Bit)), 'I7', Array(8, In(Bit)), 'O0', Array(8, Out(Bit)), 'O1', Array(8, Out(Bit)), 'O2', Array(8, Out(Bit)), 'O3', Array(8, Out(Bit)), 'O4', Array(8, Out(Bit)), 'O5', Array(8, Out(Bit)), 'O6', Array(8, Out(Bit)), 'O7', Array(8, Out(Bit)), 'valid_data_in', In(Bit), 'ready_data_in', Out(Bit), 'valid_data_out', Out(Bit), 'ready_data_out', In(Bit), ] + ClockInterface(has_ce=True)
convolution_32x32Im_2x2Win_8px_in_per_clk = DefineCircuit('convolution_32x32Im_2x2Win_8px_in_per_clk_Circuit', *args)
magmaInstance0 = DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 8, 1, 2, 2, 32, 32, 1, 1, 0, 0)()
magmaInstance1 = DefineCoreirConst(8, 1)()
magmaInstance2 = DefineCoreirConst(8, 1)()
magmaInstance3 = DefineCoreirConst(8, 1)()
magmaInstance4 = DefineCoreirConst(8, 1)()
magmaInstance5 = DefineCoreirConst(8, 1)()
magmaInstance6 = DefineCoreirConst(8, 1)()
magmaInstance7 = DefineCoreirConst(8, 1)()
magmaInstance8 = DefineCoreirConst(8, 1)()
magmaInstance9 = DefineCoreirConst(8, 2)()
magmaInstance10 = DefineCoreirConst(8, 2)()
magmaInstance11 = DefineCoreirConst(8, 2)()
magmaInstance12 = DefineCoreirConst(8, 2)()
magmaInstance13 = DefineCoreirConst(8, 2)()
magmaInstance14 = DefineCoreirConst(8, 2)()
magmaInstance15 = DefineCoreirConst(8, 2)()
magmaInstance16 = DefineCoreirConst(8, 2)()
magmaInstance24 = DefineCoreirConst(8, 2)()
magmaInstance25 = DefineCoreirConst(8, 2)()
magmaInstance26 = DefineCoreirConst(8, 2)()
magmaInstance27 = DefineCoreirConst(8, 2)()
magmaInstance28 = DefineCoreirConst(8, 2)()
magmaInstance29 = DefineCoreirConst(8, 2)()
magmaInstance30 = DefineCoreirConst(8, 2)()
magmaInstance31 = DefineCoreirConst(8, 2)()
magmaInstance32 = DefineCoreirConst(8, 1)()
magmaInstance33 = DefineCoreirConst(8, 1)()
magmaInstance34 = DefineCoreirConst(8, 1)()
magmaInstance35 = DefineCoreirConst(8, 1)()
magmaInstance36 = DefineCoreirConst(8, 1)()
magmaInstance37 = DefineCoreirConst(8, 1)()
magmaInstance38 = DefineCoreirConst(8, 1)()
magmaInstance39 = DefineCoreirConst(8, 1)()
magmaInstance61 = DefineCoreirMul(8)()
magmaInstance62 = DefineCoreirMul(8)()
magmaInstance63 = DefineCoreirMul(8)()
magmaInstance64 = DefineCoreirMul(8)()
magmaInstance65 = DefineCoreirMul(8)()
magmaInstance66 = DefineCoreirMul(8)()
magmaInstance67 = DefineCoreirMul(8)()
magmaInstance68 = DefineCoreirMul(8)()
magmaInstance69 = DefineCoreirMul(8)()
magmaInstance70 = DefineCoreirMul(8)()
magmaInstance71 = DefineCoreirMul(8)()
magmaInstance72 = DefineCoreirMul(8)()
magmaInstance73 = DefineCoreirMul(8)()
magmaInstance74 = DefineCoreirMul(8)()
magmaInstance75 = DefineCoreirMul(8)()
magmaInstance76 = DefineCoreirMul(8)()
magmaInstance77 = DefineCoreirMul(8)()
magmaInstance78 = DefineCoreirMul(8)()
magmaInstance79 = DefineCoreirMul(8)()
magmaInstance80 = DefineCoreirMul(8)()
magmaInstance81 = DefineCoreirMul(8)()
magmaInstance82 = DefineCoreirMul(8)()
magmaInstance83 = DefineCoreirMul(8)()
magmaInstance84 = DefineCoreirMul(8)()
magmaInstance85 = DefineCoreirMul(8)()
magmaInstance86 = DefineCoreirMul(8)()
magmaInstance87 = DefineCoreirMul(8)()
magmaInstance88 = DefineCoreirMul(8)()
magmaInstance89 = DefineCoreirMul(8)()
magmaInstance90 = DefineCoreirMul(8)()
magmaInstance91 = DefineCoreirMul(8)()
magmaInstance92 = DefineCoreirMul(8)()
wire(magmaInstance0.O[0][0][0], magmaInstance61.I0)
wire(magmaInstance1.O, magmaInstance61.I1)
wire(magmaInstance0.O[0][0][1], magmaInstance62.I0)
wire(magmaInstance9.O, magmaInstance62.I1)
wire(magmaInstance0.O[0][1][0], magmaInstance63.I0)
wire(magmaInstance24.O, magmaInstance63.I1)
wire(magmaInstance0.O[0][1][1], magmaInstance64.I0)
wire(magmaInstance32.O, magmaInstance64.I1)
wire(magmaInstance0.O[1][0][0], magmaInstance65.I0)
wire(magmaInstance2.O, magmaInstance65.I1)
wire(magmaInstance0.O[1][0][1], magmaInstance66.I0)
wire(magmaInstance10.O, magmaInstance66.I1)
wire(magmaInstance0.O[1][1][0], magmaInstance67.I0)
wire(magmaInstance25.O, magmaInstance67.I1)
wire(magmaInstance0.O[1][1][1], magmaInstance68.I0)
wire(magmaInstance33.O, magmaInstance68.I1)
wire(magmaInstance0.O[2][0][0], magmaInstance69.I0)
wire(magmaInstance3.O, magmaInstance69.I1)
wire(magmaInstance0.O[2][0][1], magmaInstance70.I0)
wire(magmaInstance11.O, magmaInstance70.I1)
wire(magmaInstance0.O[2][1][0], magmaInstance71.I0)
wire(magmaInstance26.O, magmaInstance71.I1)
wire(magmaInstance0.O[2][1][1], magmaInstance72.I0)
wire(magmaInstance34.O, magmaInstance72.I1)
wire(magmaInstance0.O[3][0][0], magmaInstance73.I0)
wire(magmaInstance4.O, magmaInstance73.I1)
wire(magmaInstance0.O[3][0][1], magmaInstance74.I0)
wire(magmaInstance12.O, magmaInstance74.I1)
wire(magmaInstance0.O[3][1][0], magmaInstance75.I0)
wire(magmaInstance27.O, magmaInstance75.I1)
wire(magmaInstance0.O[3][1][1], magmaInstance76.I0)
wire(magmaInstance35.O, magmaInstance76.I1)
wire(magmaInstance0.O[4][0][0], magmaInstance77.I0)
wire(magmaInstance5.O, magmaInstance77.I1)
wire(magmaInstance0.O[4][0][1], magmaInstance78.I0)
wire(magmaInstance13.O, magmaInstance78.I1)
wire(magmaInstance0.O[4][1][0], magmaInstance79.I0)
wire(magmaInstance28.O, magmaInstance79.I1)
wire(magmaInstance0.O[4][1][1], magmaInstance80.I0)
wire(magmaInstance36.O, magmaInstance80.I1)
wire(magmaInstance0.O[5][0][0], magmaInstance81.I0)
wire(magmaInstance6.O, magmaInstance81.I1)
wire(magmaInstance0.O[5][0][1], magmaInstance82.I0)
wire(magmaInstance14.O, magmaInstance82.I1)
wire(magmaInstance0.O[5][1][0], magmaInstance83.I0)
wire(magmaInstance29.O, magmaInstance83.I1)
wire(magmaInstance0.O[5][1][1], magmaInstance84.I0)
wire(magmaInstance37.O, magmaInstance84.I1)
wire(magmaInstance0.O[6][0][0], magmaInstance85.I0)
wire(magmaInstance7.O, magmaInstance85.I1)
wire(magmaInstance0.O[6][0][1], magmaInstance86.I0)
wire(magmaInstance15.O, magmaInstance86.I1)
wire(magmaInstance0.O[6][1][0], magmaInstance87.I0)
wire(magmaInstance30.O, magmaInstance87.I1)
wire(magmaInstance0.O[6][1][1], magmaInstance88.I0)
wire(magmaInstance38.O, magmaInstance88.I1)
wire(magmaInstance0.O[7][0][0], magmaInstance89.I0)
wire(magmaInstance8.O, magmaInstance89.I1)
wire(magmaInstance0.O[7][0][1], magmaInstance90.I0)
wire(magmaInstance16.O, magmaInstance90.I1)
wire(magmaInstance0.O[7][1][0], magmaInstance91.I0)
wire(magmaInstance31.O, magmaInstance91.I1)
wire(magmaInstance0.O[7][1][1], magmaInstance92.I0)
wire(magmaInstance39.O, magmaInstance92.I1)
magmaInstance93 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance94 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance95 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance96 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance97 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance98 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance99 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance100 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance93_identityGen = DefineCoreirConst(8, 0)()
wire(magmaInstance93_identityGen.O, magmaInstance93.I.identity)
wire(magmaInstance93_identityGen.O, magmaInstance94.I.identity)
wire(magmaInstance93_identityGen.O, magmaInstance95.I.identity)
wire(magmaInstance93_identityGen.O, magmaInstance96.I.identity)
wire(magmaInstance93_identityGen.O, magmaInstance97.I.identity)
wire(magmaInstance93_identityGen.O, magmaInstance98.I.identity)
wire(magmaInstance93_identityGen.O, magmaInstance99.I.identity)
wire(magmaInstance93_identityGen.O, magmaInstance100.I.identity)
wire(magmaInstance61.O, magmaInstance93.I.data[0])
wire(magmaInstance62.O, magmaInstance93.I.data[1])
wire(magmaInstance63.O, magmaInstance93.I.data[2])
wire(magmaInstance64.O, magmaInstance93.I.data[3])
wire(magmaInstance65.O, magmaInstance94.I.data[0])
wire(magmaInstance66.O, magmaInstance94.I.data[1])
wire(magmaInstance67.O, magmaInstance94.I.data[2])
wire(magmaInstance68.O, magmaInstance94.I.data[3])
wire(magmaInstance69.O, magmaInstance95.I.data[0])
wire(magmaInstance70.O, magmaInstance95.I.data[1])
wire(magmaInstance71.O, magmaInstance95.I.data[2])
wire(magmaInstance72.O, magmaInstance95.I.data[3])
wire(magmaInstance73.O, magmaInstance96.I.data[0])
wire(magmaInstance74.O, magmaInstance96.I.data[1])
wire(magmaInstance75.O, magmaInstance96.I.data[2])
wire(magmaInstance76.O, magmaInstance96.I.data[3])
wire(magmaInstance77.O, magmaInstance97.I.data[0])
wire(magmaInstance78.O, magmaInstance97.I.data[1])
wire(magmaInstance79.O, magmaInstance97.I.data[2])
wire(magmaInstance80.O, magmaInstance97.I.data[3])
wire(magmaInstance81.O, magmaInstance98.I.data[0])
wire(magmaInstance82.O, magmaInstance98.I.data[1])
wire(magmaInstance83.O, magmaInstance98.I.data[2])
wire(magmaInstance84.O, magmaInstance98.I.data[3])
wire(magmaInstance85.O, magmaInstance99.I.data[0])
wire(magmaInstance86.O, magmaInstance99.I.data[1])
wire(magmaInstance87.O, magmaInstance99.I.data[2])
wire(magmaInstance88.O, magmaInstance99.I.data[3])
wire(magmaInstance89.O, magmaInstance100.I.data[0])
wire(magmaInstance90.O, magmaInstance100.I.data[1])
wire(magmaInstance91.O, magmaInstance100.I.data[2])
wire(magmaInstance92.O, magmaInstance100.I.data[3])
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.I0, magmaInstance0.I[0][0])
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.I1, magmaInstance0.I[0][1])
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.I2, magmaInstance0.I[0][2])
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.I3, magmaInstance0.I[0][3])
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.I4, magmaInstance0.I[0][4])
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.I5, magmaInstance0.I[0][5])
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.I6, magmaInstance0.I[0][6])
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.I7, magmaInstance0.I[0][7])
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.O0, magmaInstance93.out)
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.O1, magmaInstance94.out)
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.O2, magmaInstance95.out)
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.O3, magmaInstance96.out)
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.O4, magmaInstance97.out)
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.O5, magmaInstance98.out)
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.O6, magmaInstance99.out)
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.O7, magmaInstance100.out)
wire(magmaInstance0.ready, convolution_32x32Im_2x2Win_8px_in_per_clk.ready_data_in)
wire(magmaInstance0.valid, convolution_32x32Im_2x2Win_8px_in_per_clk.valid_data_out)
wire(convolution_32x32Im_2x2Win_8px_in_per_clk.valid_data_in & convolution_32x32Im_2x2Win_8px_in_per_clk.ready_data_out & bit(convolution_32x32Im_2x2Win_8px_in_per_clk.CE), magmaInstance0.CE)
ceTerm = TermAnyType(cirb, Enable)
wire(ceTerm.I, convolution_32x32Im_2x2Win_8px_in_per_clk.CE)
EndCircuit()
