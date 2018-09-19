from aetherling.modules.reduce import ReduceSequential, ReduceParallel, renameCircuitForReduce
from magma.backend.coreir_ import CoreIRBackend
from magma.bitutils import *
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from mantle.coreir.arith import *
from mantle.coreir import DefineCoreirConst

def test_reduce_parallel():
    width = 11
    numIn = 13
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    inType = In(Array(numIn, Array(width, BitIn)))
    outType = Out(Array(width, Bit))
    args = ['I', inType, 'O', outType] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test_Reduce_Parallel', *args)

    reducePar = ReduceParallel(cirb, numIn, renameCircuitForReduce(DefineAdd(width)))
    coreirConst = DefineCoreirConst(width, 0)()
    wire(reducePar.I.data, testcircuit.I)
    wire(reducePar.I.identity, coreirConst.O)
    wire(testcircuit.O, reducePar.out)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(numIn):
        sim.set_value(testcircuit.I[i], int2seq(i, width), scope)
    sim.evaluate()
    assert seq2int(sim.get_value(testcircuit.O, scope)) == sum(range(numIn))

def test_reduce_sequential():
    width = 11
    numIn = 13
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    inType = In(Array(width, BitIn))
    outType = Out(Array(width, Bit))
    args = ['I', inType, 'O', outType, 'valid', Out(Bit)] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test_Reduce_Parallel', *args)

    reduceSeq = ReduceSequential(cirb, numIn, renameCircuitForReduce(DefineAdd(width)))
    wire(reduceSeq.I, testcircuit.I)
    wire(testcircuit.O, reduceSeq.out)
    wire(testcircuit.valid, reduceSeq.valid)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(numIn):
        sim.set_value(testcircuit.I, int2seq(i, width), scope)
        sim.evaluate()
        # exit last time of loop without going to next iteration, time to check results then
        if i < numIn - 1:
            assert sim.get_value(testcircuit.valid, scope) == False
            sim.advance_cycle()
            sim.evaluate()
    assert seq2int(sim.get_value(testcircuit.O, scope)) == sum(range(numIn))
    assert sim.get_value(testcircuit.valid, scope) == True

