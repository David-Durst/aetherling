from aetherling.modules.upsample import UpsampleSequential, UpsampleParallel
from magma import *
from magma.clock import *
from magma.backend.coreir_ import CoreIRBackend, compile
from magma.bitutils import *
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from mantle.common.countermod import CounterModM, Decode

def test_up_parallel():
    width = 5
    numOut = 2
    testVal = 3
    c = coreir.Context()
    scope = Scope()
    inType = Array[width, In(BitIn)]
    outType = Array[numOut, Out(inType)]
    args = ['I', inType, 'O', outType] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test', *args)

    upParallel = UpsampleParallel(numOut, inType)
    wire(upParallel.I, testcircuit.I)
    wire(testcircuit.O, upParallel.O)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    sim.set_value(testcircuit.I, int2seq(testVal, width), scope)
    sim.evaluate()
    sim.advance_cycle()
    sim.evaluate()
    for i in range(numOut):
        assert seq2int(sim.get_value(testcircuit.O[i], scope)) == testVal

def test_modm_counter():
    width = 5
    numCycles = 4
    maxCounter = 4
    scope = Scope()
    args = ['O', Out(Bit)] + ClockInterface(False, False)
    testcircuit = DefineCircuit('TestModM', *args)
    counter = CounterModM(maxCounter, width, cout=False)
    decode = Decode(0, width)
    wire(decode.I, counter.O)
    wire(testcircuit.O, decode.O)
    EndCircuit()
    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    for i in range(maxCounter * numCycles):
        sim.evaluate()
        sim.advance_cycle()

    assert sim.get_value(testcircuit.O, scope) == True


def test_up_sequential():
    width = 5
    numOut = 2
    testVal = 3
    c = coreir.Context()
    scope = Scope()
    inType = Array[width, In(BitIn)]
    outType = Out(inType)
    args = ['I', inType, 'O', outType, 'READY', Out(Bit)] + ClockInterface(False, False)

    testcircuit = DefineCircuit('TestUpSequential', *args)

    coreir_backend = CoreIRBackend(c)
    upSequential = UpsampleSequential(coreir_backend, numOut, inType)
    wire(upSequential.I, testcircuit.I)
    wire(testcircuit.O, upSequential.O)
    wire(testcircuit.READY, upSequential.READY)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=coreir_backend.context)

    sim.set_value(testcircuit.I, int2seq(testVal, width), scope)
    sim.evaluate()

    for i in range(numOut):
        assert seq2int(sim.get_value(testcircuit.O, scope)) == testVal
        sim.advance_cycle()
        sim.evaluate()
        assert sim.get_value(testcircuit.READY, scope) == (i == numOut - 1)


if __name__ == "__main__":
    test_up_parallel()
    test_modm_counter()
    test_up_sequential()

"""

width = 5
numElements = 1
testVal = 3
c = coreir.Context()
scope = Scope()
inType = Array[width, In(BitIn)]
#outType = Array[width, Out(BitIn)] #uncomment this line
#outType = Array[numElements, Out(inType)]
outType = Array[numElements, Out(Array[width, Bit])] #comment this line
args = ['I', inType, 'O', outType] + ClockInterface(False, False)

testcircuit = DefineCircuit('Test', *args)

#upParallel = UpParallel(numElements, inType)
#wire(upParallel.I, testcircuit.I)
#wire(testcircuit.O, upParallel.O)
#wire(testcircuit.O, testcircuit.I) # uncomment this line
wire(testcircuit.O[0], testcircuit.I) # comment this line


EndCircuit()

sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

#coreir_testcircuit = compile(testcircuit, context=c)

#c.run_passes(["rungenerators", "verifyconnectivity-onlyinputs-noclkrst",
                    #"wireclocks-coreir", "flatten", "flattentypes", "verifyconnectivity",
                    #"deletedeadinstances"])

#sim_testcircuit = coreir.SimulatorState(coreir_testcircuit)


#sim_testcircuit.set_value(["self.I"], bit_vector.BitVector(width, testVal).as_bool_list())
#sim = PythonSimulator(testcircuit)
sim.set_value(testcircuit.I, int2seq(testVal, width), scope)
sim = CoreIRSimulator(testcircuit, testcircuit.CLK)
simulate(testcircuit, CoreIRSimulator)
"""