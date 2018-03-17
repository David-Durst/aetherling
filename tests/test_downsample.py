from aetherling.modules.downsample import DownsampleSequential, DownsampleParallel
from magma.simulator import PythonSimulator
from magma import *
from magma.backend import coreir_compile
from magma.clock import *
from magma.backend.coreir_ import CoreIRBackend
from magma.bitutils import *
import bit_vector.bit_vector
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from magma.simulator.mdb import simulate
from mantle import CounterModM, Decode


def test_downsample_parallel():
    width = 5
    numIn = 2
    testVal = 3
    c = coreir.Context()
    coreir_backend = CoreIRBackend(c)
    scope = Scope()
    outType = Array(width, Out(BitIn))
    inType = Array(numIn, In(outType))
    args = ['I', inType, 'O', outType] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test_Downsample_Parallel', *args)

    downsampleParallel = DownsampleParallel(coreir_backend, numIn, inType.T)
    wire(downsampleParallel.I, testcircuit.I)
    wire(testcircuit.O, downsampleParallel.O)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=coreir_backend.context)

    for i in range(numIn):
        sim.set_value(testcircuit.I[i], int2seq(testVal, width), scope)
    sim.evaluate()
    assert seq2int(sim.get_value(testcircuit.O, scope)) == testVal


def disable_test_up_sequential():
    width = 5
    numOut = 2
    testVal = 3
    c = coreir.Context()
    scope = Scope()
    inType = Array(width, In(BitIn))
    outType = Out(inType)
    args = ['I', inType, 'O', outType, 'READY', Out(Bit)] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test_Downsample_Sequential', *args)

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
inType = Array(width, In(BitIn))
#outType = Array(width, Out(BitIn)) #uncomment this line
#outType = Array(numElements, Out(inType))
outType = Array(numElements, Out(Array(width, Bit))) #comment this line
args = ['I', inType, 'O', outType] + ClockInterface(False, False)

testcircuit = DefineCircuit('Test', *args)

#upParallel = UpParallel(numElements, inType)
#wire(upParallel.I, testcircuit.I)
#wire(testcircuit.O, upParallel.O)
#wire(testcircuit.O, testcircuit.I) # uncomment this line
wire(testcircuit.O[0], testcircuit.I) # comment this line


EndCircuit()

sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

#coreir_testcircuit = coreir_compile(testcircuit, context=c)

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