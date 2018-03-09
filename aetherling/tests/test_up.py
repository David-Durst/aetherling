from aetherling.modules.up import UpSequential, UpParallel
from magma.simulator import PythonSimulator
from magma import *
from magma.bitutils import *

def test_up_parallel():
    width = 5
    numElements = 7
    testVal = 3
    inType = Array(width, In(Bit))
    outType = Array(numElements, inType)
    args = ['I', inType, 'O', outType]

    testcircuit = DefineCircuit('Test', *args)

    upParallel = UpParallel(numElements, inType)
    wire(testcircuit.I, upParallel.I)
    wire(upParallel.O, testcircuit.O)

    EndCircuit()

    sim = PythonSimulator(testcircuit)
    sim.set_value(testcircuit.I, int2seq(testVal, width))
    sim.evaluate()
    for i in range(numElements):
        assert seq2int(sim.get_value(testcircuit.O)) == testVal

    assert False