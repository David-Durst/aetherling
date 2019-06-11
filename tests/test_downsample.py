from aetherling.modules.downsample import DownsampleSequential, DownsampleParallel
from magma.frontend.coreir_ import GetCoreIRModule
from magma import *
from magma.clock import *
from magma.backend.coreir_ import CoreIRBackend
from magma.bitutils import *
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope


def test_downsample_parallel():
    width = 5
    numIn = 2
    testVal = 3
    scope = Scope()
    outType = Array[width, Out(BitIn)]
    inType = Array[numIn, In(outType)]
    args = ['I', inType, 'O', outType] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test_Downsample_Parallel', *args)

    downsampleParallel = DownsampleParallel(numIn, inType.T)
    wire(downsampleParallel.I, testcircuit.I)
    wire(testcircuit.O, downsampleParallel.O)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(numIn):
        sim.set_value(testcircuit.I[i], int2seq(testVal, width), scope)
    sim.evaluate()
    assert seq2int(sim.get_value(testcircuit.O, scope)) == testVal


def test_downsample_sequential():
    width = 5
    numOut = 2
    testVal = 3
    scope = Scope()
    outType = Array[width, Out(BitIn)]
    inType = In(outType)
    args = ['I', inType, 'O', outType, 'VALID', Out(Bit)] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test_Downsample_Sequential', *args)

    downsampleSequential = DownsampleSequential(numOut, inType)
    wire(downsampleSequential.I, testcircuit.I)
    wire(testcircuit.O, downsampleSequential.O)
    wire(testcircuit.VALID, downsampleSequential.VALID)
    EndCircuit()

    #testModule = GetCoreIRModule(cirb, testcircuit)
    #cirb.context.run_passes(["rungenerators", "verifyconnectivity --noclkrst"], ["aetherlinglib", "commonlib", "mantle", "coreir", "global"])
    #testModule.save_to_file("downsampleSequential.json")

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

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
