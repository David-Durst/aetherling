from aetherling.modules.partition import Partition
from magma.simulator import PythonSimulator
from magma import *
from magma.backend import coreir_compile
from magma.clock import *
from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import GetCoreIRModule
from magma.bitutils import *
import bit_vector.bit_vector
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope

def test_partition():
    width = 8
    parallelism = 2
    testValInt = 210
    testValBits = int2seq(testValInt)
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    inType = Array(width, In(BitIn))
    outType = Array(parallelism, Out(Bit))
    args = ['I', inType, 'O', outType] + ClockInterface(False, False)

    testcircuit = DefineCircuit('test_partition', *args)

    part = Partition(cirb, inType, parallelism)
    wire(part.I, testcircuit.I)
    wire(testcircuit.O, part.O)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.set_value(testcircuit.I, int2seq(testValInt, width), scope)
    sim.evaluate()
    for i in range(int(width / parallelism)):
        assert seq2int(sim.get_value(testcircuit.O, scope)) == \
               seq2int(testValBits[i*parallelism:(i+1)*parallelism])
        sim.advance_cycle()
        sim.evaluate()