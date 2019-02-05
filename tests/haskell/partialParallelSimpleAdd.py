from aetherling.modules.reduce import DefineReduceSequential, DefineReduceParallel, renameCircuitForReduce
from aetherling.modules.term_any_type import TermAnyType
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

args = ['O0', Array(8, Out(Bit)), 'O1', Array(8, Out(Bit)), 'valid_data_in', In(Bit), 'ready_data_in', Out(Bit), 'valid_data_out', Out(Bit), 'ready_data_out', In(Bit), ] + ClockInterface(has_ce=True)
partialParallelSimpleAdd = DefineCircuit('partialParallelSimpleAdd_Circuit', *args)
magmaInstance0 = DefineCoreirConst(8, 3)()
magmaInstance1 = DefineCoreirConst(8, 3)()
magmaInstance2 = DefineCoreirConst(8, 2)()
magmaInstance3 = DefineCoreirConst(8, 2)()
magmaInstance5 = DefineAdd(8)()
magmaInstance6 = DefineAdd(8)()
wire(magmaInstance0.O, magmaInstance5.I0)
wire(magmaInstance2.O, magmaInstance5.I1)
wire(magmaInstance1.O, magmaInstance6.I0)
wire(magmaInstance3.O, magmaInstance6.I1)
wire(partialParallelSimpleAdd.O0, magmaInstance5.O)
wire(partialParallelSimpleAdd.O1, magmaInstance6.O)
wire(partialParallelSimpleAdd.ready_data_in, partialParallelSimpleAdd.ready_data_out)
wire(partialParallelSimpleAdd.valid_data_in, partialParallelSimpleAdd.valid_data_out)
ceTerm = TermAnyType(cirb, Enable)
wire(ceTerm.I, partialParallelSimpleAdd.CE)
EndCircuit()
