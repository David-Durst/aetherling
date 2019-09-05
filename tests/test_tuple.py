from aetherling.space_time.tuple import AtomTupleCreator, SSeqTupleCreator, SSeqTupleAppender
from aetherling.space_time.space_time_types import ST_Int, ST_Bit, ST_TSeq, ST_SSeq_Tuple, ST_Atom_Tuple
from magma import *
from magma.clock import *
from magma.bitutils import *
from magma.simulator.coreir_simulator import CoreIRSimulator
from magma.scope import Scope

def test_sseqTupleCreator():
    width = 8
    scope = Scope()
    testvals = [1, 2]
    T = ST_TSeq(2, 0, ST_Int())
    T_magma = T.magma_repr()
    args = ['I0', In(T_magma), 'I1', In(T_magma), 'O', Out(Array[2, T_magma])] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test', *args)

    sseqTupleCreator = SSeqTupleCreator(T_magma)

    wire(testcircuit.I0, sseqTupleCreator.I0)
    wire(testcircuit.I1, sseqTupleCreator.I1)
    wire(testcircuit.O, sseqTupleCreator.O)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    sim.set_value(testcircuit.I0, int2seq(testvals[0], width), scope)
    sim.set_value(testcircuit.I1, int2seq(testvals[1], width), scope)
    sim.evaluate()
    for i in range(len(testvals)):
        assert seq2int(sim.get_value(testcircuit.O[i], scope)) == testvals[i]

def test_sseqTupleAppender():
    width = 8
    scope = Scope()
    testvals = [1, 2, 3]
    T_inner = ST_TSeq(2, 0, ST_Int())
    T_outer_input = ST_SSeq_Tuple(2, ST_Int())
    T_outer_output = ST_SSeq_Tuple(3, ST_Int())
    args = ['I0', In(T_outer_input.magma_repr()), 'I1', In(T_inner.magma_repr()), \
            'O', Out(T_outer_output.magma_repr())] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test', *args)

    sseqTupleAppender = SSeqTupleAppender(T_inner.magma_repr(), 2)

    wire(testcircuit.I0, sseqTupleAppender.I0)
    wire(testcircuit.I1, sseqTupleAppender.I1)
    wire(testcircuit.O, sseqTupleAppender.O)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    sim.set_value(testcircuit.I0[0], int2seq(testvals[0], width), scope)
    sim.set_value(testcircuit.I0[1], int2seq(testvals[1], width), scope)
    sim.set_value(testcircuit.I1, int2seq(testvals[2], width), scope)
    sim.evaluate()
    for i in range(len(testvals)):
        assert seq2int(sim.get_value(testcircuit.O[i], scope)) == testvals[i]


def test_atomTupleAppender():
    width = 8
    scope = Scope()
    T0 = ST_Int()
    T1 = ST_Bit()
    T_outer_output = ST_Atom_Tuple(T0, T1)
    args = ['I0', In(T0.magma_repr()), 'I1', In(T1.magma_repr()), \
            'O', Out(T_outer_output.magma_repr())] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test', *args)

    sseqTupleAppender = AtomTupleCreator(T0.magma_repr(), T1.magma_repr())

    wire(testcircuit.I0, sseqTupleAppender.I0)
    wire(testcircuit.I1, sseqTupleAppender.I1)
    wire(testcircuit.O, sseqTupleAppender.O)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    sim.set_value(testcircuit.I0, int2seq(4, width), scope)
    sim.set_value(testcircuit.I1, True, scope)
    sim.evaluate()
    assert seq2int(sim.get_value(testcircuit.O[0], scope)) == 4
    assert sim.get_value(testcircuit.O[1], scope) == True
