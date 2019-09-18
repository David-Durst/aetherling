from magma import *
from magma.circuit import DefineCircuitKind
from mantle.coreir.arith import *
from mantle.coreir.logic import *
from mantle.coreir.compare import *
from aetherling.space_time.space_time_types import *
from aetherling.helpers.nameCleanup import cleanName

@cache_definition
def DefineSSeqTupleCreator(t: ST_Type) -> DefineCircuitKind:
    """
    A module for creating an SSeq Tuple of length 2.
    The T is producer by calling magma_repr on Space-Time type T'.
    T' -> T' -> SSeq 2 T'

    I0 : In(T)
    I1 : In(T)
    O : Out(Array[2, T])
    """

    class _SeqTupleCreator(Circuit):
        name = "sseqTupleCreator_t{}".format(cleanName(str(t)))
        binary_op = True
        st_in_t = [t, t]
        st_out_t = ST_SSeq_Tuple(2, t)

        IO = ["I0", In(t.magma_repr()), "I1", In(t.magma_repr()), "O", Out(st_out_t.magma_repr())]

        @classmethod
        def definition(sseq_tuple_creator):
            wire(sseq_tuple_creator.I0, sseq_tuple_creator.O[0])
            wire(sseq_tuple_creator.I1, sseq_tuple_creator.O[1])

    return _SeqTupleCreator

def SSeqTupleCreator(t: ST_Type):
    return DefineSSeqTupleCreator(t)()

@cache_definition
def DefineSSeqTupleAppender(t: ST_Type, n: int) -> DefineCircuitKind:
    """
    A module for creating an SSeq Tuple of length (n+1) out of one of length n.
    The T is producer by calling magma_repr on Space-Time type T'.
    SSeq n T' -> T' -> SSeq 2 T'

    I0 : In(Array[n, T])
    I1 : In(T)
    O : Out(Array[n+1, T])
    """

    class _SeqTupleAppender(Circuit):
        name = "sseqTupleAppender_t{}_n{}".format(cleanName(str(t)), str(n))

        binary_op = True
        st_in_t = [ST_SSeq(n, t), t]
        st_out_t = ST_SSeq(n+1, t)
        IO = ["I0", In(st_in_t[0].magma_repr()),
              "I1", In(st_in_t[1].magma_repr()),
              "O", Out(st_out_t.magma_repr())]

        @classmethod
        def definition(sseq_tuple_appender):
            for i in range(n):
                wire(sseq_tuple_appender.I0[i], sseq_tuple_appender.O[i])
            wire(sseq_tuple_appender.I1, sseq_tuple_appender.O[n])

    return _SeqTupleAppender

def SSeqTupleAppender(t: ST_Type, n: int):
    return DefineSSeqTupleAppender(t, n)()

@cache_definition
def DefineAtomTupleCreator(t0: ST_Type, t1: ST_Type) -> DefineCircuitKind:
    """
    A module for creating an Atom Tuple
    The Ts are producer by calling magma_repr on Space-Time types T's.
    T0' -> T1' -> T0' x T1'

    I0 : In(T0)
    I1 : In(T1)
    O : Out(Tuple(T0, T1)
    """

    class _AtomTupleCreator(Circuit):
        name = "atomTupleCreator_t0{}_t1{}".format(cleanName(str(t0)), cleanName(str(t1)))

        binary_op = True
        st_in_t = [t0, t1]
        st_out_t = ST_Atom_Tuple(t0, t1)
        IO = ["I0", In(st_in_t[0].magma_repr()),
              "I1", In(st_in_t[1].magma_repr()),
              "O", Out(st_out_t.magma_repr())]

        @classmethod
        def definition(atom_tuple_creator):
            wire(atom_tuple_creator.I0, atom_tuple_creator.O[0])
            wire(atom_tuple_creator.I1, atom_tuple_creator.O[1])

    return _AtomTupleCreator

def AtomTupleCreator(t0: ST_Type, t1: ST_Type):
    return DefineAtomTupleCreator(t0, t1)()

