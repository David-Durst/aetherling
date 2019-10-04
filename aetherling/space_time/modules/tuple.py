from magma import *
from magma.circuit import DefineCircuitKind
from mantle.coreir.arith import *
from mantle.coreir.logic import *
from mantle.coreir.compare import *
from aetherling.space_time.space_time_types import *
from aetherling.helpers.nameCleanup import cleanName
from aetherling.space_time.type_helpers import valid_ports
from aetherling.modules.term_any_type import TermAnyType

@cache_definition
def DefineSSeqTupleCreator(t: ST_Type, has_valid: bool = False) -> DefineCircuitKind:
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
        if has_valid:
            IO += valid_ports

        @classmethod
        def definition(sseq_tuple_creator):
            wire(sseq_tuple_creator.I0, sseq_tuple_creator.O[0])
            wire(sseq_tuple_creator.I1, sseq_tuple_creator.O[1])
            if has_valid:
                wire(sseq_tuple_creator.valid_up, sseq_tuple_creator.valid_down)

    return _SeqTupleCreator

def SSeqTupleCreator(t: ST_Type):
    return DefineSSeqTupleCreator(t)()

@cache_definition
def DefineSSeqTupleAppender(t: ST_Type, n: int, has_valid: bool = False) -> DefineCircuitKind:
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
        st_in_t = [ST_SSeq_Tuple(n, t), t]
        st_out_t = ST_SSeq_Tuple(n+1, t)
        IO = ["I0", In(st_in_t[0].magma_repr()),
              "I1", In(st_in_t[1].magma_repr()),
              "O", Out(st_out_t.magma_repr())]
        if has_valid:
            IO += valid_ports

        @classmethod
        def definition(sseq_tuple_appender):
            for i in range(n):
                wire(sseq_tuple_appender.I0[i], sseq_tuple_appender.O[i])
            wire(sseq_tuple_appender.I1, sseq_tuple_appender.O[n])
            if has_valid:
                wire(sseq_tuple_appender.valid_up, sseq_tuple_appender.valid_down)

    return _SeqTupleAppender

def SSeqTupleAppender(t: ST_Type, n: int):
    return DefineSSeqTupleAppender(t, n)()

@cache_definition
def DefineSSeqToSTuple(t: ST_Type, n: int, has_valid: bool = False) -> DefineCircuitKind:
    """
    A module for converting an SSeq to an SSeq Tuple
    SSeq n t -> STuple n t

    I : In(SSeq(n, t).magma_repr())
    O : In(STuple(n, t).magma_repr())
    """

    class _SSeqToTuple(Circuit):
        name = "sseqToStuple_t{}_n{}".format(cleanName(str(t)), str(n))

        binary_op = False
        st_in_t = [ST_SSeq_Tuple(n, t)]
        st_out_t = ST_SSeq(n, t)
        IO = ["I", In(st_in_t[0].magma_repr()),
              "O", Out(st_out_t.magma_repr())]
        if has_valid:
            IO += valid_ports

        @classmethod
        def definition(cls):
            wire(cls.I, cls.O)
            if has_valid:
                wire(cls.valid_up, cls.valid_down)

    return _SSeqToTuple

def SSeqToSTuple(t: ST_Type, n: int, has_valid: bool = False):
    return DefineSSeqToSTuple(t, n, has_valid)()

@cache_definition
def DefineSTupleToSSeq(t: ST_Type, n: int, has_valid: bool = False) -> DefineCircuitKind:
    """
    A module for converting an SSeq Tuple to an SSeq
    SSeq n t -> STuple n t

    I : In(STuple(n, t).magma_repr())
    O : Out(SSeq(n, t).magma_repr())
    """

    class _TupleToSSeq(Circuit):
        name = "stupleToSSeq_t{}_n{}".format(cleanName(str(t)), str(n))

        binary_op = False
        st_in_t = [ST_SSeq_Tuple(n, t)]
        st_out_t = ST_SSeq(n, t)
        IO = ["I", In(st_in_t[0].magma_repr()),
              "O", Out(st_out_t.magma_repr())]
        if has_valid:
            IO += valid_ports

        @classmethod
        def definition(cls):
            wire(cls.I, cls.O)
            if has_valid:
                wire(cls.valid_up, cls.valid_down)

    return _TupleToSSeq


def STupleToSSeq(t: ST_Type, n: int, has_valid: bool = False):
    return DefineSTupleToSSeq(t, n, has_valid)()


@cache_definition
def DefineAtomTupleCreator(t0: ST_Type, t1: ST_Type, has_valid: bool = False) -> DefineCircuitKind:
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
        if has_valid:
            IO += valid_ports

        @classmethod
        def definition(atom_tuple_creator):
            wire(atom_tuple_creator.I0, atom_tuple_creator.O[0])
            wire(atom_tuple_creator.I1, atom_tuple_creator.O[1])
            if has_valid:
                wire(atom_tuple_creator.valid_up, atom_tuple_creator.valid_down)

    return _AtomTupleCreator

def AtomTupleCreator(t0: ST_Type, t1: ST_Type):
    return DefineAtomTupleCreator(t0, t1)()

@cache_definition
def DefineFst(t: ST_Type, has_valid: bool = False) -> DefineCircuitKind:
    """
    A module for taking the first elemnt of an atom tuple
    t x t' -> t

    I : In(Tuple(t, t'))
    O : Out(t)
    """

    class _Fst(Circuit):
        name = "fst_t{}".format(cleanName(str(t)))

        binary_op = False
        st_in_t = [t]
        st_out_t = t.t0
        IO = ["I", In(st_in_t[0].magma_repr()),
              "O", Out(st_out_t.magma_repr())]
        if has_valid:
            IO += valid_ports

        @classmethod
        def definition(cls):
            wire(cls.I[0], cls.O)
            snd_term = TermAnyType(t.t1.magma_repr())
            wire(cls.I[1], snd_term.I)
            if has_valid:
                wire(cls.valid_up, cls.valid_down)

    return _Fst

def Fst(t: ST_Type):
    return DefineFst(t)()

@cache_definition
def DefineSnd(t: ST_Type, has_valid: bool = False) -> DefineCircuitKind:
    """
    A module for taking the second elemnt of an atom tuple
    t x t' -> t'

    I : In(Tuple(t, t'))
    O : Out(t')
    """

    class _Snd(Circuit):
        name = "snd_t{}".format(cleanName(str(t)))

        binary_op = False
        st_in_t = [t]
        st_out_t = t.t1
        IO = ["I", In(st_in_t[0].magma_repr()),
              "O", Out(st_out_t.magma_repr())]
        if has_valid:
            IO += valid_ports

        @classmethod
        def definition(cls):
            fst_term = TermAnyType(t.t0.magma_repr())
            wire(cls.I[0], fst_term.I)
            wire(cls.I[1], cls.O)
            if has_valid:
                wire(cls.valid_up, cls.valid_down)

    return _Snd

def Snd(t: ST_Type):
    return DefineSnd(t)()
