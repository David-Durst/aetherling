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

args = ['I0', Array(8, In(Bit)), 'I1', Array(8, In(Bit)), 'O0', Array(8, Out(Bit)), 'valid_data_in', In(Bit), 'ready_data_in', Out(Bit), 'valid_data_out', Out(Bit), 'ready_data_out', In(Bit), ] + ClockInterface(has_ce=True)
downsample_256x256_to_32x32_2px_in_per_clk = DefineCircuit('downsample_256x256_to_32x32_2px_in_per_clk_Circuit', *args)
magmaInstance0 = DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 2, 1, 2, 2, 256, 256, 2, 2, 0, 0)()
magmaInstance1 = DefineNoop(DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 2, 1, 2, 2, 256, 256, 2, 2, 0, 0))()
magmaInstance2 = DefineCoreirConst(8, 1)()
magmaInstance3 = DefineCoreirConst(8, 2)()
magmaInstance4 = DefineCoreirConst(8, 3)()
magmaInstance5 = DefineCoreirConst(8, 4)()
wire(magmaInstance0.O[0][0][0], magmaInstance1.in_O[0][0][0])
wire(magmaInstance0.O[0][0][1], magmaInstance1.in_O[0][0][1])
wire(magmaInstance0.O[0][1][0], magmaInstance1.in_O[0][1][0])
wire(magmaInstance0.O[0][1][1], magmaInstance1.in_O[0][1][1])
magmaInstance6 = DefineCoreirMul(8)()
magmaInstance7 = DefineCoreirMul(8)()
magmaInstance8 = DefineCoreirMul(8)()
magmaInstance9 = DefineCoreirMul(8)()
wire(magmaInstance1.O[0][0][0], magmaInstance6.I0)
wire(magmaInstance2.O, magmaInstance6.I1)
wire(magmaInstance1.O[0][0][1], magmaInstance7.I0)
wire(magmaInstance3.O, magmaInstance7.I1)
wire(magmaInstance1.O[0][1][0], magmaInstance8.I0)
wire(magmaInstance4.O, magmaInstance8.I1)
wire(magmaInstance1.O[0][1][1], magmaInstance9.I0)
wire(magmaInstance5.O, magmaInstance9.I1)
magmaInstance10 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance10_identityGen = DefineCoreirConst(8, 0)()
wire(magmaInstance10_identityGen.O, magmaInstance10.I.identity)
wire(magmaInstance6.O, magmaInstance10.I.data[0])
wire(magmaInstance7.O, magmaInstance10.I.data[1])
wire(magmaInstance8.O, magmaInstance10.I.data[2])
wire(magmaInstance9.O, magmaInstance10.I.data[3])
magmaInstance11 = DefineNoop(DefineCoreirConst(8, 0))()
magmaInstance12 = DefineCoreirConst(8, 4)()
wire(magmaInstance10.out, magmaInstance11.in_O)
magmaInstance13 = DefineCoreirUDiv(8)()
wire(magmaInstance11.O, magmaInstance13.I0)
wire(magmaInstance12.O, magmaInstance13.I1)
magmaInstance14 = DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 1, 1, 2, 2, 128, 128, 2, 2, 0, 0)()
wire(magmaInstance13.O, magmaInstance14.I[0][0])
magmaInstance15 = DefineNoop(DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 1, 1, 2, 2, 128, 128, 2, 2, 0, 0))()
magmaInstance16 = DefineCoreirConst(8, 1)()
magmaInstance17 = DefineCoreirConst(8, 2)()
magmaInstance18 = DefineCoreirConst(8, 3)()
magmaInstance19 = DefineCoreirConst(8, 4)()
wire(magmaInstance14.O[0][0][0], magmaInstance15.in_O[0][0][0])
wire(magmaInstance14.O[0][0][1], magmaInstance15.in_O[0][0][1])
wire(magmaInstance14.O[0][1][0], magmaInstance15.in_O[0][1][0])
wire(magmaInstance14.O[0][1][1], magmaInstance15.in_O[0][1][1])
magmaInstance20 = DefineCoreirMul(8)()
magmaInstance21 = DefineCoreirMul(8)()
magmaInstance22 = DefineCoreirMul(8)()
magmaInstance23 = DefineCoreirMul(8)()
wire(magmaInstance15.O[0][0][0], magmaInstance20.I0)
wire(magmaInstance16.O, magmaInstance20.I1)
wire(magmaInstance15.O[0][0][1], magmaInstance21.I0)
wire(magmaInstance17.O, magmaInstance21.I1)
wire(magmaInstance15.O[0][1][0], magmaInstance22.I0)
wire(magmaInstance18.O, magmaInstance22.I1)
wire(magmaInstance15.O[0][1][1], magmaInstance23.I0)
wire(magmaInstance19.O, magmaInstance23.I1)
magmaInstance24 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance24_identityGen = DefineCoreirConst(8, 0)()
wire(magmaInstance24_identityGen.O, magmaInstance24.I.identity)
wire(magmaInstance20.O, magmaInstance24.I.data[0])
wire(magmaInstance21.O, magmaInstance24.I.data[1])
wire(magmaInstance22.O, magmaInstance24.I.data[2])
wire(magmaInstance23.O, magmaInstance24.I.data[3])
magmaInstance25 = DefineNoop(DefineCoreirConst(8, 0))()
magmaInstance26 = DefineCoreirConst(8, 4)()
wire(magmaInstance24.out, magmaInstance25.in_O)
magmaInstance27 = DefineCoreirUDiv(8)()
wire(magmaInstance25.O, magmaInstance27.I0)
wire(magmaInstance26.O, magmaInstance27.I1)
magmaInstance28 = DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 1, 1, 2, 2, 64, 64, 2, 2, 0, 0)()
wire(magmaInstance27.O, magmaInstance28.I[0][0])
magmaInstance29 = DefineNoop(DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 1, 1, 2, 2, 64, 64, 2, 2, 0, 0))()
magmaInstance30 = DefineCoreirConst(8, 1)()
magmaInstance31 = DefineCoreirConst(8, 2)()
magmaInstance32 = DefineCoreirConst(8, 3)()
magmaInstance33 = DefineCoreirConst(8, 4)()
wire(magmaInstance28.O[0][0][0], magmaInstance29.in_O[0][0][0])
wire(magmaInstance28.O[0][0][1], magmaInstance29.in_O[0][0][1])
wire(magmaInstance28.O[0][1][0], magmaInstance29.in_O[0][1][0])
wire(magmaInstance28.O[0][1][1], magmaInstance29.in_O[0][1][1])
magmaInstance34 = DefineCoreirMul(8)()
magmaInstance35 = DefineCoreirMul(8)()
magmaInstance36 = DefineCoreirMul(8)()
magmaInstance37 = DefineCoreirMul(8)()
wire(magmaInstance29.O[0][0][0], magmaInstance34.I0)
wire(magmaInstance30.O, magmaInstance34.I1)
wire(magmaInstance29.O[0][0][1], magmaInstance35.I0)
wire(magmaInstance31.O, magmaInstance35.I1)
wire(magmaInstance29.O[0][1][0], magmaInstance36.I0)
wire(magmaInstance32.O, magmaInstance36.I1)
wire(magmaInstance29.O[0][1][1], magmaInstance37.I0)
wire(magmaInstance33.O, magmaInstance37.I1)
magmaInstance38 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance38_identityGen = DefineCoreirConst(8, 0)()
wire(magmaInstance38_identityGen.O, magmaInstance38.I.identity)
wire(magmaInstance34.O, magmaInstance38.I.data[0])
wire(magmaInstance35.O, magmaInstance38.I.data[1])
wire(magmaInstance36.O, magmaInstance38.I.data[2])
wire(magmaInstance37.O, magmaInstance38.I.data[3])
magmaInstance39 = DefineNoop(DefineCoreirConst(8, 0))()
magmaInstance40 = DefineCoreirConst(8, 4)()
wire(magmaInstance38.out, magmaInstance39.in_O)
magmaInstance41 = DefineCoreirUDiv(8)()
wire(magmaInstance39.O, magmaInstance41.I0)
wire(magmaInstance40.O, magmaInstance41.I1)
wire(downsample_256x256_to_32x32_2px_in_per_clk.I0, magmaInstance0.I[0][0])
wire(downsample_256x256_to_32x32_2px_in_per_clk.I1, magmaInstance0.I[0][1])
wire(downsample_256x256_to_32x32_2px_in_per_clk.O0, magmaInstance41.O)
wire(magmaInstance0.ready, downsample_256x256_to_32x32_2px_in_per_clk.ready_data_in)
wire(magmaInstance28.valid, downsample_256x256_to_32x32_2px_in_per_clk.valid_data_out)
wire(downsample_256x256_to_32x32_2px_in_per_clk.valid_data_in & magmaInstance14.ready & bit(downsample_256x256_to_32x32_2px_in_per_clk.CE), magmaInstance0.CE)
wire(magmaInstance0.valid & magmaInstance28.ready & bit(downsample_256x256_to_32x32_2px_in_per_clk.CE), magmaInstance14.CE)
wire(magmaInstance14.valid & downsample_256x256_to_32x32_2px_in_per_clk.ready_data_out & bit(downsample_256x256_to_32x32_2px_in_per_clk.CE), magmaInstance28.CE)
ceTerm = TermAnyType(cirb, Enable)
wire(ceTerm.I, downsample_256x256_to_32x32_2px_in_per_clk.CE)
EndCircuit()