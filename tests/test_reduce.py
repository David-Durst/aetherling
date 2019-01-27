from aetherling.modules.reduce import ReduceSequential, ReduceParallel, ReducePartiallyParallel, renameCircuitForReduce
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


def test_reduce_partially_parallel():
    width = 11
    numIn = 12
    parallelism = 3
    c = coreir.Context()
    cirb = CoreIRBackend(c)
    scope = Scope()
    inType = In(Array(width, BitIn))
    outType = Out(Array(width, Bit))
    args = ['I', Array(parallelism, inType), 'O', outType, 'valid', Out(Bit)] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test_Reduce_Parallel', *args)

    reduce_op = ReducePartiallyParallel(cirb, numIn, parallelism, renameCircuitForReduce(DefineAdd(width)))
    wire(reduce_op.I, testcircuit.I)
    wire(testcircuit.O, reduce_op.O)
    coreirConst = DefineCoreirConst(width, 0)()
    wire(reduce_op.identity, coreirConst.O)
    wire(testcircuit.valid, reduce_op.valid)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK, context=cirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    reduce_result = 0
    for i in range(numIn * 2 // parallelism):
        reduce_result += i + i * 5 + i * 10
        sim.set_value(testcircuit.I[0], int2seq(i, width), scope)
        sim.set_value(testcircuit.I[1], int2seq(i*5, width), scope)
        sim.set_value(testcircuit.I[2], int2seq(i*10, width), scope)
        sim.evaluate()
        # exit last time of loop without going to next iteration, time to check results then
        if i % (numIn // parallelism) == (numIn // parallelism) - 1:
            assert seq2int(sim.get_value(testcircuit.O, scope)) == reduce_result
            assert sim.get_value(testcircuit.valid, scope) == True
            reduce_result = 0
        else:
            assert sim.get_value(testcircuit.valid, scope) == False
        sim.advance_cycle()
        sim.evaluate()
