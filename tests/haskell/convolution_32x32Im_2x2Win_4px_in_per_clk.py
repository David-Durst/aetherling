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

args = ['I0', Array(8, In(Bit)), 'I1', Array(8, In(Bit)), 'I2', Array(8, In(Bit)), 'I3', Array(8, In(Bit)), 'O0', Array(8, Out(Bit)), 'O1', Array(8, Out(Bit)), 'O2', Array(8, Out(Bit)), 'O3', Array(8, Out(Bit)), 'O4', Array(8, Out(Bit)), 'O5', Array(8, Out(Bit)), 'O6', Array(8, Out(Bit)), 'O7', Array(8, Out(Bit)), 'O8', Array(8, Out(Bit)), 'O9', Array(8, Out(Bit)), 'O10', Array(8, Out(Bit)), 'O11', Array(8, Out(Bit)), 'O12', Array(8, Out(Bit)), 'O13', Array(8, Out(Bit)), 'O14', Array(8, Out(Bit)), 'O15', Array(8, Out(Bit)), 'valid_data_in', In(Bit), 'ready_data_in', Out(Bit), 'valid_data_out', Out(Bit), 'ready_data_out', In(Bit), ] + ClockInterface(has_ce=True)
convolution_32x32Im_2x2Win_4px_in_per_clk = DefineCircuit('convolution_32x32Im_2x2Win_4px_in_per_clk_Circuit', *args)
magmaInstance0 = DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 4, 1, 2, 2, 32, 32, 1, 1, 0, 0)()
magmaInstance1 = DefineCoreirConst(8, 1)()
magmaInstance2 = DefineCoreirConst(8, 1)()
magmaInstance3 = DefineCoreirConst(8, 1)()
magmaInstance4 = DefineCoreirConst(8, 1)()
magmaInstance5 = DefineCoreirConst(8, 2)()
magmaInstance6 = DefineCoreirConst(8, 2)()
magmaInstance7 = DefineCoreirConst(8, 2)()
magmaInstance8 = DefineCoreirConst(8, 2)()
magmaInstance12 = DefineCoreirConst(8, 2)()
magmaInstance13 = DefineCoreirConst(8, 2)()
magmaInstance14 = DefineCoreirConst(8, 2)()
magmaInstance15 = DefineCoreirConst(8, 2)()
magmaInstance16 = DefineCoreirConst(8, 1)()
magmaInstance17 = DefineCoreirConst(8, 1)()
magmaInstance18 = DefineCoreirConst(8, 1)()
magmaInstance19 = DefineCoreirConst(8, 1)()
magmaInstance29 = DefineCoreirMul(8)()
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
wire(magmaInstance0.O[0][0][0], magmaInstance29.I0)
wire(magmaInstance1.O, magmaInstance29.I1)
wire(magmaInstance0.O[0][0][1], magmaInstance30.I0)
wire(magmaInstance5.O, magmaInstance30.I1)
wire(magmaInstance0.O[0][1][0], magmaInstance31.I0)
wire(magmaInstance12.O, magmaInstance31.I1)
wire(magmaInstance0.O[0][1][1], magmaInstance32.I0)
wire(magmaInstance16.O, magmaInstance32.I1)
wire(magmaInstance0.O[1][0][0], magmaInstance33.I0)
wire(magmaInstance2.O, magmaInstance33.I1)
wire(magmaInstance0.O[1][0][1], magmaInstance34.I0)
wire(magmaInstance6.O, magmaInstance34.I1)
wire(magmaInstance0.O[1][1][0], magmaInstance35.I0)
wire(magmaInstance13.O, magmaInstance35.I1)
wire(magmaInstance0.O[1][1][1], magmaInstance36.I0)
wire(magmaInstance17.O, magmaInstance36.I1)
wire(magmaInstance0.O[2][0][0], magmaInstance37.I0)
wire(magmaInstance3.O, magmaInstance37.I1)
wire(magmaInstance0.O[2][0][1], magmaInstance38.I0)
wire(magmaInstance7.O, magmaInstance38.I1)
wire(magmaInstance0.O[2][1][0], magmaInstance39.I0)
wire(magmaInstance14.O, magmaInstance39.I1)
wire(magmaInstance0.O[2][1][1], magmaInstance40.I0)
wire(magmaInstance18.O, magmaInstance40.I1)
wire(magmaInstance0.O[3][0][0], magmaInstance41.I0)
wire(magmaInstance4.O, magmaInstance41.I1)
wire(magmaInstance0.O[3][0][1], magmaInstance42.I0)
wire(magmaInstance8.O, magmaInstance42.I1)
wire(magmaInstance0.O[3][1][0], magmaInstance43.I0)
wire(magmaInstance15.O, magmaInstance43.I1)
wire(magmaInstance0.O[3][1][1], magmaInstance44.I0)
wire(magmaInstance19.O, magmaInstance44.I1)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.I0, magmaInstance0.I[0][0])
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.I1, magmaInstance0.I[0][1])
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.I2, magmaInstance0.I[0][2])
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.I3, magmaInstance0.I[0][3])
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.O0, magmaInstance29.O)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.O1, magmaInstance30.O)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.O2, magmaInstance31.O)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.O3, magmaInstance32.O)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.O4, magmaInstance33.O)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.O5, magmaInstance34.O)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.O6, magmaInstance35.O)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.O7, magmaInstance36.O)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.O8, magmaInstance37.O)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.O9, magmaInstance38.O)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.O10, magmaInstance39.O)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.O11, magmaInstance40.O)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.O12, magmaInstance41.O)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.O13, magmaInstance42.O)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.O14, magmaInstance43.O)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.O15, magmaInstance44.O)
wire(magmaInstance0.ready, convolution_32x32Im_2x2Win_4px_in_per_clk.ready_data_in)
wire(magmaInstance0.valid, convolution_32x32Im_2x2Win_4px_in_per_clk.valid_data_out)
wire(convolution_32x32Im_2x2Win_4px_in_per_clk.valid_data_in & convolution_32x32Im_2x2Win_4px_in_per_clk.ready_data_out & bit(convolution_32x32Im_2x2Win_4px_in_per_clk.CE), magmaInstance0.CE)
ceTerm = TermAnyType(cirb, Enable)
wire(ceTerm.I, convolution_32x32Im_2x2Win_4px_in_per_clk.CE)
EndCircuit()
