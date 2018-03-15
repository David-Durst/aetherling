from magma import *
from magma.clock import *
from magma.bitutils import *
from magma.simulator.coreir_simulator import CoreIRSimulator
from magma.scope import Scope
from mantle import CounterModM

def test_modm_counter():
    width = 5
    scope = Scope()
    args = ['O', Out(Bit)] + ClockInterface(False, False)
    testcircuit = DefineCircuit('TestModM', *args)
    counter = CounterModM(2, 5)
    wire(testcircuit.O, counter.COUT)
    EndCircuit()
    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    sim.evaluate()
    sim.advance()
    sim.evaluate()
    assert seq2int(sim.get_value(testcircuit.O, scope)) == 0


if __name__ == "__main__":
    test_modm_counter()

