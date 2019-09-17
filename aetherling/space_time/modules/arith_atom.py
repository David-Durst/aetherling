from mantle.coreir.arith import *
from mantle.coreir.logic import *
from mantle.coreir.compare import *
from mantle.coreir import DefineCoreirConst
from aetherling.space_time.space_time_types import *
from aetherling.modules.mux_any_type import DefineMuxAnyType

int_width = ST_Int().magma_repr().size()
bit_width = ST_Bit().magma_repr().size()

@cache_definition
def DefineAbs_Atom():
    class _Abs(Circuit):
        name = "Abs_Atom"
        IO = ['I', In(ST_Int().magma_repr()), 'O', Out(ST_Int().magma_repr())]
        binary_op = False
        st_in_t = ST_Int()
        st_out_t = ST_Int()
        @classmethod
        def definition(cls):
            neg = DefineNegate(int_width)
            cmp = DefineCoreirUgt(int_width)()
            mux = DefineMuxAnyType(ST_Int().magma_repr(), 2)
            wire(cls.I, mux.I[0])
            wire(cls.I, neg.I)
            wire(neg.O, mux.I[1])
            wire(cls.I, cmp.I0)
            wire(neg.O, cmp.I1)
            wire(cmp.O, mux.sel[0])
            wire(mux.O, cls.O)
    return _Abs

@cache_definition
def DefineNot_Atom():
    class _Not(Circuit):
        name = "Not_Atom"
        IO = ['I', In(ST_Bit().magma_repr()), 'O', Out(ST_Bit().magma_repr())]
        binary_op = False
        st_in_t = ST_Bit()
        st_out_t = ST_Bit()
        @classmethod
        def definition(cls):
            op = DefineNegate(bit_width)
            wire(cls.I, op.I)
            wire(op.O, cls.O)
    return _Not

@cache_definition
def DefineAdd_Atom():
    class _Add(Circuit):
        name = "Add_Atom"
        IO = ['I', In(ST_Atom_Tuple(ST_Int(), ST_Int()).magma_repr()),
              'O', Out(ST_Int().magma_repr())]
        binary_op = False
        st_in_t = ST_Atom_Tuple(ST_Int(), ST_Int())
        st_out_t = ST_Int()
        @classmethod
        def definition(cls):
            op = DefineAdd(int_width)
            wire(cls.I[0], op.I0)
            wire(cls.I[1], op.I1)
            wire(op.O, cls.O)
    return _Add

@cache_definition
def DefineSub_Atom():
    class _Sub(Circuit):
        name = "Sub_Atom"
        IO = ['I', In(ST_Atom_Tuple(ST_Int(), ST_Int()).magma_repr()),
              'O', Out(ST_Int().magma_repr())]
        binary_op = False
        st_in_t = ST_Atom_Tuple(ST_Int(), ST_Int())
        st_out_t = ST_Int()
        @classmethod
        def definition(cls):
            op = DefineSub(int_width)
            wire(cls.I[0], op.I0)
            wire(cls.I[1], op.I1)
            wire(op.O, cls.O)
    return _Sub

@cache_definition
def DefineMul_Atom():
    class _Mul(Circuit):
        name = "Mul_Atom"
        IO = ['I', In(ST_Atom_Tuple(ST_Int(), ST_Int()).magma_repr()),
              'O', Out(ST_Int().magma_repr())]
        binary_op = False
        st_in_t = ST_Atom_Tuple(ST_Int(), ST_Int())
        st_out_t = ST_Int()
        @classmethod
        def definition(cls):
            op = DefineMul(int_width)
            wire(cls.I[0], op.I0)
            wire(cls.I[1], op.I1)
            wire(op.O, cls.O)
    return _Mul

@cache_definition
def DefineDiv_Atom():
    class _Div(Circuit):
        name = "Div_Atom"
        IO = ['I', In(ST_Atom_Tuple(ST_Int(), ST_Int()).magma_repr()),
              'O', Out(ST_Int().magma_repr())]
        binary_op = False
        st_in_t = ST_Atom_Tuple(ST_Int(), ST_Int())
        st_out_t = ST_Int()
        @classmethod
        def definition(cls):
            op = DefineUDiv(int_width)
            wire(cls.I[0], op.I0)
            wire(cls.I[1], op.I1)
            wire(op.O, cls.O)
    return _Div

@cache_definition
def DefineEq_Atom(t: ST_Type):
    class _Eq(Circuit):
        name = "Eq_Atom"
        IO = ['I', In(ST_Atom_Tuple(t, t).magma_repr()),
              'O', Out(ST_Bit().magma_repr())]
        binary_op = False
        st_in_t = ST_Atom_Tuple(ST_Int(), ST_Int())
        st_out_t = ST_Bit()
        @classmethod
        def definition(cls):
            op = DefineEQ(t.magma_repr().size())
            wire(cls.I[0], op.I0)
            wire(cls.I[1], op.I1)
            wire(op.O, cls.O)
    return _Eq
