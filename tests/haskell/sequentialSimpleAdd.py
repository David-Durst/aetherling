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

args = ['O0', Array(8, Out(Bit)), 'valid_data_in', In(Bit), 'ready_data_in', Out(Bit), 'valid_data_out', Out(Bit), 'ready_data_out', In(Bit), ] + ClockInterface(has_ce=True)
sequentialSimpleAdd = DefineCircuit('sequentialSimpleAdd_Circuit', *args)
magmaInstance0 = DefineCoreirConst(8, 3)()
magmaInstance1 = DefineCoreirConst(8, 2)()
magmaInstance2 = DefineAdd(8)()
wire(magmaInstance0.O, magmaInstance2.I0)
wire(magmaInstance1.O, magmaInstance2.I1)
wire(sequentialSimpleAdd.O0, magmaInstance2.O)
wire(sequentialSimpleAdd.ready_data_out, sequentialSimpleAdd.ready_data_in)
wire(sequentialSimpleAdd.valid_data_in, sequentialSimpleAdd.valid_data_out)
ceTerm = TermAnyType(cirb, Enable)
wire(ceTerm.I, sequentialSimpleAdd.CE)
EndCircuit()
