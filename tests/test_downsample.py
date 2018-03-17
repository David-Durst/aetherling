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


def test_downsample_sequential():
    width = 5
    numOut = 2
    testVal = 3
    c = coreir.Context()
    scope = Scope()
    outType = Array(width, Out(BitIn))
    inType = In(outType)
    args = ['I', inType, 'O', outType, 'VALID', Out(Bit)] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test_Downsample_Sequential', *args)

    coreir_backend = CoreIRBackend(c)
    upSequential = DownsampleSequential(numOut, inType)
    wire(upSequential.I, testcircuit.I)
    wire(testcircuit.O, upSequential.O)
    wire(testcircuit.VALID, upSequential.VALID)
    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=coreir_backend.context)

    sim.set_value(testcircuit.I, int2seq(testVal, width), scope)
    sim.evaluate()

    for i in range(numOut):
        assert seq2int(sim.get_value(testcircuit.O, scope)) == testVal
        assert sim.get_value(testcircuit.VALID, scope) == (i == 0)
        sim.advance_cycle()
        sim.evaluate()



if __name__ == "__main__":
    test_downsample_parallel()
    test_downsample_sequential()
