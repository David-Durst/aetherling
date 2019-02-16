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

args = ['I0', Array(8, In(Bit)), 'I1', Array(8, In(Bit)), 'O0', Array(8, Out(Bit)), 'O1', Array(8, Out(Bit)), 'O2', Array(8, Out(Bit)), 'O3', Array(8, Out(Bit)), 'O4', Array(8, Out(Bit)), 'O5', Array(8, Out(Bit)), 'O6', Array(8, Out(Bit)), 'O7', Array(8, Out(Bit)), 'valid_data_in', In(Bit), 'ready_data_in', Out(Bit), 'valid_data_out', Out(Bit), 'ready_data_out', In(Bit), ] + ClockInterface(has_ce=True)
convolution_32x32Im_2x2Win_2px_in_per_clk = DefineCircuit('convolution_32x32Im_2x2Win_2px_in_per_clk_Circuit', *args)
magmaInstance0 = DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 2, 1, 2, 2, 32, 32, 1, 1, 0, 0)()
magmaInstance1 = DefineCoreirConst(8, 1)()
magmaInstance2 = DefineCoreirConst(8, 1)()
magmaInstance3 = DefineCoreirConst(8, 2)()
magmaInstance4 = DefineCoreirConst(8, 2)()
magmaInstance6 = DefineCoreirConst(8, 2)()
magmaInstance7 = DefineCoreirConst(8, 2)()
magmaInstance8 = DefineCoreirConst(8, 1)()
magmaInstance9 = DefineCoreirConst(8, 1)()
magmaInstance13 = DefineCoreirMul(8)()
magmaInstance14 = DefineCoreirMul(8)()
magmaInstance15 = DefineCoreirMul(8)()
magmaInstance16 = DefineCoreirMul(8)()
magmaInstance17 = DefineCoreirMul(8)()
magmaInstance18 = DefineCoreirMul(8)()
magmaInstance19 = DefineCoreirMul(8)()
magmaInstance20 = DefineCoreirMul(8)()
wire(magmaInstance0.O[0][0][0], magmaInstance13.I0)
wire(magmaInstance1.O, magmaInstance13.I1)
wire(magmaInstance0.O[0][0][1], magmaInstance14.I0)
wire(magmaInstance3.O, magmaInstance14.I1)
wire(magmaInstance0.O[0][1][0], magmaInstance15.I0)
wire(magmaInstance6.O, magmaInstance15.I1)
wire(magmaInstance0.O[0][1][1], magmaInstance16.I0)
wire(magmaInstance8.O, magmaInstance16.I1)
wire(magmaInstance0.O[1][0][0], magmaInstance17.I0)
wire(magmaInstance2.O, magmaInstance17.I1)
wire(magmaInstance0.O[1][0][1], magmaInstance18.I0)
wire(magmaInstance4.O, magmaInstance18.I1)
wire(magmaInstance0.O[1][1][0], magmaInstance19.I0)
wire(magmaInstance7.O, magmaInstance19.I1)
wire(magmaInstance0.O[1][1][1], magmaInstance20.I0)
wire(magmaInstance9.O, magmaInstance20.I1)
wire(convolution_32x32Im_2x2Win_2px_in_per_clk.I0, magmaInstance0.I[0][0])
wire(convolution_32x32Im_2x2Win_2px_in_per_clk.I1, magmaInstance0.I[0][1])
wire(convolution_32x32Im_2x2Win_2px_in_per_clk.O0, magmaInstance13.O)
wire(convolution_32x32Im_2x2Win_2px_in_per_clk.O1, magmaInstance14.O)
wire(convolution_32x32Im_2x2Win_2px_in_per_clk.O2, magmaInstance15.O)
wire(convolution_32x32Im_2x2Win_2px_in_per_clk.O3, magmaInstance16.O)
wire(convolution_32x32Im_2x2Win_2px_in_per_clk.O4, magmaInstance17.O)
wire(convolution_32x32Im_2x2Win_2px_in_per_clk.O5, magmaInstance18.O)
wire(convolution_32x32Im_2x2Win_2px_in_per_clk.O6, magmaInstance19.O)
wire(convolution_32x32Im_2x2Win_2px_in_per_clk.O7, magmaInstance20.O)
wire(magmaInstance0.ready, convolution_32x32Im_2x2Win_2px_in_per_clk.ready_data_in)
wire(magmaInstance0.valid, convolution_32x32Im_2x2Win_2px_in_per_clk.valid_data_out)
wire(convolution_32x32Im_2x2Win_2px_in_per_clk.valid_data_in & convolution_32x32Im_2x2Win_2px_in_per_clk.ready_data_out & bit(convolution_32x32Im_2x2Win_2px_in_per_clk.CE), magmaInstance0.CE)
ceTerm = TermAnyType(cirb, Enable)
wire(ceTerm.I, convolution_32x32Im_2x2Win_2px_in_per_clk.CE)
EndCircuit()
