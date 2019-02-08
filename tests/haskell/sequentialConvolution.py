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

args = ['I0', Array(8, In(Bit)), 'O0', Array(8, Out(Bit)), 'valid_data_in', In(Bit), 'ready_data_in', Out(Bit), 'valid_data_out', Out(Bit), 'ready_data_out', In(Bit), ] + ClockInterface(has_ce=True)
sequentialConvolution = DefineCircuit('sequentialConvolution_Circuit', *args)
magmaInstance0 = DefineTwoDimensionalLineBuffer(cirb, Array(8, In(Bit)), 1, 1, 2, 2, 8, 8, 1, 1, 0, 0)()
magmaInstance1 = DefineCoreirConst(8, 1)()
magmaInstance2 = DefineCoreirConst(8, 2)()
magmaInstance3 = DefineCoreirConst(8, 2)()
magmaInstance4 = DefineCoreirConst(8, 1)()
magmaInstance5 = DefineCoreirMul(8)()
magmaInstance6 = DefineCoreirMul(8)()
magmaInstance7 = DefineCoreirMul(8)()
magmaInstance8 = DefineCoreirMul(8)()
wire(magmaInstance0.O[0][0][0], magmaInstance5.I0)
wire(magmaInstance1.O, magmaInstance5.I1)
wire(magmaInstance0.O[0][0][1], magmaInstance6.I0)
wire(magmaInstance3.O, magmaInstance6.I1)
wire(magmaInstance0.O[0][1][0], magmaInstance7.I0)
wire(magmaInstance2.O, magmaInstance7.I1)
wire(magmaInstance0.O[0][1][1], magmaInstance8.I0)
wire(magmaInstance4.O, magmaInstance8.I1)
magmaInstance9 = DefineReduceParallel(cirb, 4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance9_identityGen = DefineCoreirConst(8, 0)()
wire(magmaInstance9_identityGen.O, magmaInstance9.I.identity)
wire(magmaInstance5.O, magmaInstance9.I.data[0])
wire(magmaInstance6.O, magmaInstance9.I.data[1])
wire(magmaInstance7.O, magmaInstance9.I.data[2])
wire(magmaInstance8.O, magmaInstance9.I.data[3])
wire(sequentialConvolution.I0, magmaInstance0.I[0][0])
wire(sequentialConvolution.O0, magmaInstance9.out)
wire(magmaInstance0.ready, sequentialConvolution.ready_data_in)
wire(magmaInstance0.valid, sequentialConvolution.valid_data_out)
wire(sequentialConvolution.valid_data_in & sequentialConvolution.ready_data_out & bit(sequentialConvolution.CE), magmaInstance0.CE)
ceTerm = TermAnyType(cirb, Enable)
wire(ceTerm.I, sequentialConvolution.CE)
EndCircuit()
