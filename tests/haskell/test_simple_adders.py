from aetherling.modules.reduce import ReduceSequential, ReduceParallel, ReducePartiallyParallel, renameCircuitForReduce
from magma.backend.coreir_ import CoreIRBackend
from magma.bitutils import *
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from mantle.coreir.arith import *
from mantle.coreir import DefineCoreirConst

def test_parallel_simple_add():
    from .parallelSimpleAdd import cirb as parallelSimpleAddCirb, parallelSimpleAdd
    sim = CoreIRSimulator(parallelSimpleAdd, parallelSimpleAdd.CLK, context=parallelSimpleAddCirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.set_value(parallelSimpleAdd.I0, int2seq(1, 8))
    sim.set_value(parallelSimpleAdd.I1, int2seq(2, 8))
    sim.set_value(parallelSimpleAdd.I2, int2seq(3, 8))
    sim.set_value(parallelSimpleAdd.I3, int2seq(4, 8))
    sim.evaluate()
    assert seq2int(sim.get_value(parallelSimpleAdd.O0)) == 2
    assert seq2int(sim.get_value(parallelSimpleAdd.O1)) == 3
    assert seq2int(sim.get_value(parallelSimpleAdd.O2)) == 4
    assert seq2int(sim.get_value(parallelSimpleAdd.O3)) == 5

def test_partial_parallel_simple_add():
    from .partialParallelSimpleAdd import cirb as partialParallelSimpleAddCirb, partialParallelSimpleAdd
    sim = CoreIRSimulator(partialParallelSimpleAdd, partialParallelSimpleAdd.CLK, context=partialParallelSimpleAddCirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.set_value(partialParallelSimpleAdd.I0, int2seq(3, 8))
    sim.set_value(partialParallelSimpleAdd.I1, int2seq(4, 8))
    sim.evaluate()
    assert seq2int(sim.get_value(partialParallelSimpleAdd.O0)) == 4
    assert seq2int(sim.get_value(partialParallelSimpleAdd.O1)) == 5

def test_sequential_simple_add():
    from .sequentialSimpleAdd import cirb as sequentialSimpleAddCirb, sequentialSimpleAdd
    sim = CoreIRSimulator(sequentialSimpleAdd, sequentialSimpleAdd.CLK, context=sequentialSimpleAddCirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.set_value(sequentialSimpleAdd.I0, int2seq(9, 8))
    sim.evaluate()
    assert seq2int(sim.get_value(sequentialSimpleAdd.O0)) == 10
