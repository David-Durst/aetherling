from aetherling.modules.fifo import DefineFIFO, FIFO
from magma import *
from magma.clock import *
from magma.bitutils import *
from magma.simulator.coreir_simulator import CoreIRSimulator
from magma.scope import Scope
from mantle.common.countermod import CounterModM, Decode

def test_fifo_write_then_read():
    width = 5
    num_in = 2
    time_per_element = 3
    test_vals = [i for i in range(num_in * time_per_element)]
    in_type = In(Array[width, Bit])
    scope = Scope()

    testcircuit = DefineFIFO(num_in, time_per_element, in_type, False)

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    # write first
    sim.set_value(testcircuit.valid_up, True, scope)
    sim.set_value(testcircuit.ready_down, False, scope)

    for i in range(len(test_vals)):
        sim.set_value(testcircuit.I, int2seq(test_vals[i], width), scope)
        sim.evaluate()

        assert sim.get_value(testcircuit.valid_down, scope) == (i != 0)
        assert sim.get_value(testcircuit.ready_up, scope) == True

        sim.advance_cycle()
        sim.evaluate()

    # wrote all locations, so not ready
    assert sim.get_value(testcircuit.ready_up, scope) == False
    # read second
    sim.set_value(testcircuit.valid_up, False, scope)
    sim.set_value(testcircuit.ready_down, True, scope)

    for i in range(len(test_vals)):
        sim.evaluate()
        assert sim.get_value(testcircuit.valid_down, scope) == True
        assert seq2int(sim.get_value(testcircuit.O, scope)) == test_vals[i]
        print(sim.get_value(testcircuit.nsc, scope))
        sim.advance_cycle()
        sim.evaluate()

        # after first read, ready again
        assert sim.get_value(testcircuit.ready_up) == True

    print(sim.get_value(testcircuit.nsc, scope))
    # after all reads, no longer valid
    assert sim.get_value(testcircuit.valid_down) == False
