from magma import *
from magma.clock import *
from magma.backend.coreir_ import CoreIRBackend, compile
from aetherling.modules.delayed_buffer import DefineDelayedBuffer
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from magma.bitutils import *


def test_delayed_buffer():
    width = 5
    numOut = 2
    testVal = 3
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    inType = Array(width, In(BitIn))
    outType = Out(inType)
    testcircuit = DefineDelayedBuffer(cirb, Array(8, Bit), 3, 1, 12)

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context)

    sim.set_value(testcircuit.CE, True, scope)
    last_output = 0
    last_input = 0
    # do two cycles
    for clock_index in range(24):
        # for each cycle, input every other of first half
        if clock_index % 12 < 6 and clock_index % 2 == 0:
            sim.set_value(testcircuit.WE, True, scope)
            sim.set_value(testcircuit.I[0], int2seq(last_input, 8), scope)
            last_input += 1
        else:
            sim.set_value(testcircuit.WE, False, scope)
        sim.evaluate()

        if clock_index % 4 == 0:
            assert sim.get_value(testcircuit.valid, scope) == True
            assert seq2int(sim.get_value(testcircuit.O, scope)[0]) == last_output
            last_output += 1
        else:
            assert sim.get_value(testcircuit.valid, scope) == False
        sim.advance_cycle()
        sim.evaluate()



if __name__ == "__main__":
    test_delayed_buffer()
