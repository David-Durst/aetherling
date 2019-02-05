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

args = ['O0', Array(8, Out(Bit)), 'O1', Array(8, Out(Bit)), 'O2', Array(8, Out(Bit)), 'O3', Array(8, Out(Bit)), 'valid_data_in', In(Bit), 'ready_data_in', Out(Bit), 'valid_data_out', Out(Bit), 'ready_data_out', In(Bit), ] + ClockInterface(has_ce=True)
parallelSimpleAdd = DefineCircuit('parallelSimpleAdd_Circuit', *args)
magmaInstance0 = DefineCoreirConst(8, 3)()
magmaInstance1 = DefineCoreirConst(8, 3)()
magmaInstance2 = DefineCoreirConst(8, 3)()
magmaInstance3 = DefineCoreirConst(8, 3)()
magmaInstance4 = DefineCoreirConst(8, 2)()
magmaInstance5 = DefineCoreirConst(8, 2)()
magmaInstance6 = DefineCoreirConst(8, 2)()
magmaInstance7 = DefineCoreirConst(8, 2)()
magmaInstance11 = DefineAdd(8)()
magmaInstance12 = DefineAdd(8)()
magmaInstance13 = DefineAdd(8)()
magmaInstance14 = DefineAdd(8)()
wire(magmaInstance0.O, magmaInstance11.I0)
wire(magmaInstance4.O, magmaInstance11.I1)
wire(magmaInstance1.O, magmaInstance12.I0)
wire(magmaInstance5.O, magmaInstance12.I1)
wire(magmaInstance2.O, magmaInstance13.I0)
wire(magmaInstance6.O, magmaInstance13.I1)
wire(magmaInstance3.O, magmaInstance14.I0)
wire(magmaInstance7.O, magmaInstance14.I1)
wire(parallelSimpleAdd.O0, magmaInstance11.O)
wire(parallelSimpleAdd.O1, magmaInstance12.O)
wire(parallelSimpleAdd.O2, magmaInstance13.O)
wire(parallelSimpleAdd.O3, magmaInstance14.O)
wire(parallelSimpleAdd.ready_data_in, parallelSimpleAdd.ready_data_out)
wire(parallelSimpleAdd.valid_data_in, parallelSimpleAdd.valid_data_out)
ceTerm = TermAnyType(cirb, Enable)
wire(ceTerm.I, parallelSimpleAdd.CE)
EndCircuit()
