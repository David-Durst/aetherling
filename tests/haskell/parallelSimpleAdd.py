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

args = ['I0', Array[8, In(Bit)], 'I1', Array[8, In(Bit)], 'I2', Array[8, In(Bit)], 'I3', Array[8, In(Bit)], 'O0', Array[8, Out(Bit)], 'O1', Array[8, Out(Bit)], 'O2', Array[8, Out(Bit)], 'O3', Array[8, Out(Bit)], 'valid_data_in', In(Bit), 'ready_data_in', Out(Bit), 'valid_data_out', Out(Bit), 'ready_data_out', In(Bit), ] + ClockInterface(has_ce=True)
parallelSimpleAdd = DefineCircuit('parallelSimpleAdd_Circuit', *args)
magmaInstance0 = DefineNoop(DefineCoreirConst(8, 1))()
magmaInstance1 = DefineNoop(DefineCoreirConst(8, 1))()
magmaInstance2 = DefineNoop(DefineCoreirConst(8, 1))()
magmaInstance3 = DefineNoop(DefineCoreirConst(8, 1))()
magmaInstance4 = DefineCoreirConst(8, 1)()
magmaInstance5 = DefineCoreirConst(8, 1)()
magmaInstance6 = DefineCoreirConst(8, 1)()
magmaInstance7 = DefineCoreirConst(8, 1)()
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
wire(parallelSimpleAdd.I0, magmaInstance0.in_O)
wire(parallelSimpleAdd.I1, magmaInstance1.in_O)
wire(parallelSimpleAdd.I2, magmaInstance2.in_O)
wire(parallelSimpleAdd.I3, magmaInstance3.in_O)
wire(parallelSimpleAdd.O0, magmaInstance11.O)
wire(parallelSimpleAdd.O1, magmaInstance12.O)
wire(parallelSimpleAdd.O2, magmaInstance13.O)
wire(parallelSimpleAdd.O3, magmaInstance14.O)
wire(parallelSimpleAdd.ready_data_out, parallelSimpleAdd.ready_data_in)
wire(parallelSimpleAdd.valid_data_in, parallelSimpleAdd.valid_data_out)
ceTerm = TermAnyType(cirb, Enable)
wire(ceTerm.I, parallelSimpleAdd.CE)
EndCircuit()
