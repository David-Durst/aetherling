from aetherling.modules.noop import DefineNoop
from magma import *
from mantle.coreir import DefineCoreirConst
from magma.clock import *
from magma.backend.coreir_ import CoreIRBackend, compile
from magma.bitutils import *
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from mantle.common.countermod import CounterModM, Decode

def test_noop():
    testVal = 21
    scope = Scope()
    inType = Array[8, In(Bit)]
    outType = Array[8, Out(Bit)]
    args = ['I', inType, 'O', outType] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test', *args)

    noopInst = DefineNoop(DefineCoreirConst(8, 0))()
    wire(noopInst.in_O, testcircuit.I)
    wire(testcircuit.O, noopInst.O)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    sim.set_value(testcircuit.I, int2seq(testVal, 8), scope)
    sim.evaluate()
    sim.advance_cycle()
    sim.evaluate()
    assert seq2int(sim.get_value(testcircuit.O, scope)) == testVal

