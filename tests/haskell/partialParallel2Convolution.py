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

args = ['I0', Array[8, In(Bit)], 'I1', Array[8, In(Bit)], 'O0', Array[8, Out(Bit)], 'O1', Array[8, Out(Bit)], 'valid_data_in', In(Bit), 'ready_data_in', Out(Bit), 'valid_data_out', Out(Bit), 'ready_data_out', In(Bit), ] + ClockInterface(has_ce=True)
partialParallel2Convolution = DefineCircuit('partialParallel2Convolution_Circuit', *args)
magmaInstance0 = DefineTwoDimensionalLineBuffer(Array[8, In(Bit)], 2, 1, 2, 2, 8, 8, 1, 1, 0, 0)()
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
magmaInstance21 = DefineReduceParallel(4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance22 = DefineReduceParallel(4, renameCircuitForReduce(DefineAdd(8)))()
magmaInstance21_identityGen = DefineCoreirConst(8, 0)()
wire(magmaInstance21_identityGen.O, magmaInstance21.I.identity)
wire(magmaInstance21_identityGen.O, magmaInstance22.I.identity)
wire(magmaInstance13.O, magmaInstance21.I.data[0])
wire(magmaInstance14.O, magmaInstance21.I.data[1])
wire(magmaInstance15.O, magmaInstance21.I.data[2])
wire(magmaInstance16.O, magmaInstance21.I.data[3])
wire(magmaInstance17.O, magmaInstance22.I.data[0])
wire(magmaInstance18.O, magmaInstance22.I.data[1])
wire(magmaInstance19.O, magmaInstance22.I.data[2])
wire(magmaInstance20.O, magmaInstance22.I.data[3])
wire(partialParallel2Convolution.I0, magmaInstance0.I[0][0])
wire(partialParallel2Convolution.I1, magmaInstance0.I[0][1])
wire(partialParallel2Convolution.O0, magmaInstance21.out)
wire(partialParallel2Convolution.O1, magmaInstance22.out)
wire(magmaInstance0.ready, partialParallel2Convolution.ready_data_in)
wire(magmaInstance0.valid, partialParallel2Convolution.valid_data_out)
wire(partialParallel2Convolution.valid_data_in & partialParallel2Convolution.ready_data_out & bit(partialParallel2Convolution.CE), magmaInstance0.CE)
ceTerm = TermAnyType(Enable)
wire(ceTerm.I, partialParallel2Convolution.CE)
EndCircuit()
