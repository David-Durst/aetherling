from magma import *
from magma.circuit import DefineCircuitKind
from aetherling.helpers.nameCleanup import cleanName

@cache_definition
def DefineSSeqTupleCreator(T: Kind) -> DefineCircuitKind:
    """
    A module for creating an SSeq Tuple of length 2.
    The T must be a space-time type.
    T -> T -> SSeq 2 T

    I0 : In(T)
    I1 : In(T)
    O : Out(Array[2, T])
    """

    class _SeqTupleCreator(Circuit):
        T_magma = T.magma_repr()
        name = "sseqTupleCreator_t{}".format(cleanName(str(T_magma)))

        IO = ["I0", In(T_magma), "I1", In(T_magma), "O", Out(Array[2, T_magma])]

        @classmethod
        def definition(sseq_tuple_creator):
            wire(sseq_tuple_creator.I0, sseq_tuple_creator.O[0])
            wire(sseq_tuple_creator.I1, sseq_tuple_creator.O[1])

    return _SeqTupleCreator

def SSeqTupleCreator(T: Kind):
    return DefineSSeqTupleCreator(T)()

@cache_definition
def DefineSSeqTupleAppender(T: Kind, n: int) -> DefineCircuitKind:
    """
    A module for creating an SSeq Tuple of length (n+1) out of one of length n.
    SSeq n T -> T -> SSeq 2 T

    I0 : In(Array[n, T])
    I1 : In(T)
    O : Out(Array[n+1, T])
    """

    class _SeqTupleAppender(Circuit):
        T_magma = T.magma_repr()
        name = "sseqTupleAppender_t{}_n{}".format(cleanName(str(T_magma)), str(n))

        IO = ["I0", In(Array[n, T_magma]), "I1", In(T_magma), "O", Out(Array[n+1, T_magma])]

        @classmethod
        def definition(sseq_tuple_appender):
            for i in range(n):
                wire(sseq_tuple_appender.I0[i], sseq_tuple_appender.O[i])
            wire(sseq_tuple_appender.I1, sseq_tuple_appender.O[n])

    return _SeqTupleAppender

def SSeqTupleAppender(T: Kind, n: int):
    return DefineSSeqTupleAppender(T, n)()

@cache_definition
def DefineAtomTupleCreator(T0: Kind, T1: Kind) -> DefineCircuitKind:
    """
    A module for creating an Atom Tuple
    T0 -> T1 -> T0 x T1

    I0 : In(T0)
    I1 : In(T1)
    O : Out(Tuple(T0, T1)
    """

    class _AtomTupleCreator(Circuit):
        T0_magma = T0.magma_repr()
        T1_magma = T1.magma_repr()
        name = "atomTupleCreator_t0{}_t1{}".format(cleanName(str(T0_magma)), cleanName(str(T1_magma)))

        IO = ["I0", In(T0_magma), "I1", In(T1_magma), "O", Out(Tuple(T0_magma, T1_magma))]

        @classmethod
        def definition(sseq_tuple_creator):
            wire(sseq_tuple_creator.I0, sseq_tuple_creator.O[0])
            wire(sseq_tuple_creator.I1, sseq_tuple_creator.O[1])

    return _AtomTupleCreator

def AtomTupleCreator(T0: Kind, T1: Kind):
    return DefineAtomTupleCreator(T0, T1)()
