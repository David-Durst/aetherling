from magma import *
from magma.circuit import DefineCircuitKind
from mantle.coreir.arith import *
from mantle.coreir.logic import *
from mantle.coreir.compare import *
from aetherling.space_time.space_time_types import *
from aetherling.helpers.nameCleanup import cleanName

@cache_definition
def DefineSSeqTupleCreator(T: Kind) -> DefineCircuitKind:
    """
    A module for creating an SSeq Tuple of length 2.
    The T is producer by calling magma_repr on Space-Time type T'.
    T' -> T' -> SSeq 2 T'

    I0 : In(T)
    I1 : In(T)
    O : Out(Array[2, T])
    """

    class _SeqTupleCreator(Circuit):
        name = "sseqTupleCreator_t{}".format(cleanName(str(T)))

        IO = ["I0", In(T), "I1", In(T), "O", Out(Array[2, T])]

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
    The T is producer by calling magma_repr on Space-Time type T'.
    SSeq n T' -> T' -> SSeq 2 T'

    I0 : In(Array[n, T])
    I1 : In(T)
    O : Out(Array[n+1, T])
    """

    class _SeqTupleAppender(Circuit):
        name = "sseqTupleAppender_t{}_n{}".format(cleanName(str(T)), str(n))

        IO = ["I0", In(Array[n, T]), "I1", In(T), "O", Out(Array[n+1, T])]

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
    The Ts are producer by calling magma_repr on Space-Time types T's.
    T0' -> T1' -> T0' x T1'

    I0 : In(T0)
    I1 : In(T1)
    O : Out(Tuple(T0, T1)
    """

    class _AtomTupleCreator(Circuit):
        name = "atomTupleCreator_t0{}_t1{}".format(cleanName(str(T0)), cleanName(str(T1)))

        IO = ["I0", In(T0), "I1", In(T1), "O", Out(Tuple(T0, T1))]

        @classmethod
        def definition(sseq_tuple_creator):
            wire(sseq_tuple_creator.I0, sseq_tuple_creator.O[0])
            wire(sseq_tuple_creator.I1, sseq_tuple_creator.O[1])

    return _AtomTupleCreator

def AtomTupleCreator(T0: Kind, T1: Kind):
    return DefineAtomTupleCreator(T0, T1)()

@cache_definition
def DefineAtomAdd():
    """
    A module for adding two atom ints
    Atom_Int x Atom_Int -> Atom_Int

    I : In(Tuple(ST_Int, ST_Int))
    O : Out(ST_Int)
    """

    class _AtomAdd(Circuit):
        name = "atomTupleAdd"
        int_T = ST_Int().magma_repr()

        IO = ["I", In(Tuple(int_T, int_T)), "O", Out(int_T)]

        @classmethod
        def definition(atom_add):
            adder = DefineAdd(int_width)()
            wire(atom_add.I[0], adder.I0)
            wire(atom_add.I[1], adder.I1)
            wire(adder.O, atom_add.O)

    return _AtomAdd

def AtomAdd():
    return DefineAtomAdd()()

@cache_definition
def DefineAtomSub():
    """
    A module for subtracting two atom ints
    Atom_Int x Atom_Int -> Atom_Int

    I : In(Tuple(ST_Int, ST_Int))
    O : Out(ST_Int)
    """

    class _AtomSub(Circuit):
        name = "atomTupleSub"
        int_T = ST_Int().magma_repr()

        IO = ["I", In(Tuple(int_T, int_T)), "O", Out(int_T)]

        @classmethod
        def definition(atom_sub):
            adder = DefineSub(int_width)()
            wire(atom_sub.I[0], adder.I0)
            wire(atom_sub.I[1], adder.I1)
            wire(adder.O, atom_sub.O)

    return _AtomSub

def AtomSub():
    return DefineAtomSub()()

@cache_definition
def DefineAtomMul():
    """
    A module for multiplying two atom ints
    Atom_Int x Atom_Int -> Atom_Int

    I : In(Tuple(ST_Int, ST_Int))
    O : Out(ST_Int)
    """

    class _AtomMul(Circuit):
        name = "atomTupleMul"
        int_T = ST_Int().magma_repr()

        IO = ["I", In(Tuple(int_T, int_T)), "O", Out(int_T)]

        @classmethod
        def definition(atom_mul):
            adder = DefineMul(int_width)()
            wire(atom_mul.I[0], adder.I0)
            wire(atom_mul.I[1], adder.I1)
            wire(adder.O, atom_mul.O)

    return _AtomMul

def AtomMul():
    return DefineAtomMul()()

@cache_definition
def DefineAtomDiv():
    """
    A module for multiplying two atom ints
    Atom_Int x Atom_Int -> Atom_Int

    I : In(Tuple(ST_Int, ST_Int))
    O : Out(ST_Int)
    """

    class _AtomDiv(Circuit):
        name = "atomTupleDiv"
        int_T = ST_Int().magma_repr()

        IO = ["I", In(Tuple(int_T, int_T)), "O", Out(int_T)]

        @classmethod
        def definition(atom_mul):
            adder = DefineUDiv(int_width)()
            wire(atom_mul.I[0], adder.I0)
            wire(atom_mul.I[1], adder.I1)
            wire(adder.O, atom_mul.O)

    return _AtomDiv

def AtomDiv():
    return DefineAtomDiv()()
