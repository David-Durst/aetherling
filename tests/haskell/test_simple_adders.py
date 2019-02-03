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

    sim.evaluate()
    assert seq2int(sim.get_value(parallelSimpleAdd.O0)) == 5
    assert seq2int(sim.get_value(parallelSimpleAdd.O1)) == 5
    assert seq2int(sim.get_value(parallelSimpleAdd.O2)) == 5
    assert seq2int(sim.get_value(parallelSimpleAdd.O3)) == 5

def test_partial_parallel_simple_add():
    from .partialParallelSimpleAdd import cirb as partialParallelSimpleAddCirb, partialParallelSimpleAdd
    sim = CoreIRSimulator(partialParallelSimpleAdd, partialParallelSimpleAdd.CLK, context=partialParallelSimpleAddCirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.evaluate()
    assert seq2int(sim.get_value(partialParallelSimpleAdd.O0)) == 5
    assert seq2int(sim.get_value(partialParallelSimpleAdd.O1)) == 5

def test_sequential_simple_add():
    from .sequentialSimpleAdd import cirb as sequentialSimpleAddCirb, sequentialSimpleAdd
    sim = CoreIRSimulator(sequentialSimpleAdd, sequentialSimpleAdd.CLK, context=sequentialSimpleAddCirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.evaluate()
    assert seq2int(sim.get_value(sequentialSimpleAdd.O0)) == 5
