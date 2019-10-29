from mantle.coreir.arith import *
from mantle.coreir.logic import *
from mantle.coreir.compare import *
from mantle.coreir import DefineCoreirConst
from aetherling.space_time.space_time_types import *
from aetherling.space_time.type_helpers import valid_ports
from aetherling.modules.mux_any_type import DefineMuxAnyType
from aetherling.helpers.nameCleanup import cleanName
from mantle.common.register import DefineRegister
from magma import *

int_width = ST_Int().magma_repr().size()
bit_width = ST_Bit().magma_repr().size()

@cache_definition
def DefineAbs_Atom(has_valid: bool = False):
    class _Abs(Circuit):
        name = "Abs_Atom"
        IO = ['I', In(ST_Int().magma_repr()), 'O', Out(ST_Int().magma_repr())]
        if has_valid:
            IO += valid_ports
        binary_op = False
        st_in_t = [ST_Int()]
        st_out_t = ST_Int()
        @classmethod
        def definition(cls):
            neg = DefineNegate(int_width)()
            cmp = DefineCoreirUgt(int_width)()
            mux = DefineMuxAnyType(ST_Int().magma_repr(), 2)()
            wire(cls.I, mux.data[0])
            wire(cls.I, neg.I)
            wire(neg.O, mux.data[1])
            wire(cls.I, cmp.I0)
            wire(neg.O, cmp.I1)
            wire(cmp.O, mux.sel[0])
            wire(mux.out, cls.O)
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _Abs

@cache_definition
def DefineNot_Atom(has_valid: bool = False):
    class _Not(Circuit):
        name = "Not_Atom"
        IO = ['I', In(ST_Bit().magma_repr()), 'O', Out(ST_Bit().magma_repr())]
        if has_valid:
            IO += valid_ports
        binary_op = False
        st_in_t = [ST_Bit()]
        st_out_t = ST_Bit()
        @classmethod
        def definition(cls):
            op = DefineNegate(bit_width)()
            wire(cls.I, op.I)
            wire(op.O, cls.O)
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _Not

@cache_definition
def DefineAdd_Atom(has_valid: bool = False):
    class _Add(Circuit):
        name = "Add_Atom"
        IO = ['I', In(ST_Atom_Tuple(ST_Int(), ST_Int()).magma_repr()),
              'O', Out(ST_Int().magma_repr())]
        if has_valid:
            IO += valid_ports
        binary_op = False
        st_in_t = [ST_Atom_Tuple(ST_Int(), ST_Int())]
        st_out_t = ST_Int()
        @classmethod
        def definition(cls):
            op = DefineAdd(int_width)()
            wire(cls.I[0], op.I0)
            wire(cls.I[1], op.I1)
            wire(op.O, cls.O)
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _Add

@cache_definition
def DefineSub_Atom(has_valid: bool = False):
    class _Sub(Circuit):
        name = "Sub_Atom"
        IO = ['I', In(ST_Atom_Tuple(ST_Int(), ST_Int()).magma_repr()),
              'O', Out(ST_Int().magma_repr())]
        if has_valid:
            IO += valid_ports
        binary_op = False
        st_in_t = [ST_Atom_Tuple(ST_Int(), ST_Int())]
        st_out_t = ST_Int()
        @classmethod
        def definition(cls):
            op = DefineSub(int_width)()
            wire(cls.I[0], op.I0)
            wire(cls.I[1], op.I1)
            wire(op.O, cls.O)
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _Sub

@cache_definition
def DefineMul_Atom(has_valid: bool = False):
    class _Mul(Circuit):
        name = "Mul_Atom"
        IO = ['I', In(ST_Atom_Tuple(ST_Int(), ST_Int()).magma_repr()),
              'O', Out(ST_Int().magma_repr())]
        if has_valid:
            IO += valid_ports
        binary_op = False
        st_in_t = [ST_Atom_Tuple(ST_Int(), ST_Int())]
        st_out_t = ST_Int()
        @classmethod
        def definition(cls):
            op = DefineMul(int_width)()
            wire(cls.I[0], op.I0)
            wire(cls.I[1], op.I1)
            wire(op.O, cls.O)
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _Mul

@cache_definition
def DefineDiv_Atom(has_valid: bool = False):
    class _Div(Circuit):
        name = "Div_Atom"
        IO = ['I', In(ST_Atom_Tuple(ST_Int(), ST_Int()).magma_repr()),
              'O', Out(ST_Int().magma_repr())]
        if has_valid:
            IO += valid_ports
        binary_op = False
        st_in_t = [ST_Atom_Tuple(ST_Int(), ST_Int())]
        st_out_t = ST_Int()
        @classmethod
        def definition(cls):
            op = DefineUDiv(int_width)()
            wire(cls.I[0], op.I0)
            wire(cls.I[1], op.I1)
            div_reg = DefineRegister(cls.st_out_t.magma_repr().N)()
            wire(op.O, div_reg.I)
            wire(div_reg.O, cls.O)

            if has_valid:
                valid_reg = DefineRegister(1)()
                wire(cls.valid_up, valid_reg.I[0])
                wire(valid_reg.O[0], cls.valid_down)
    return _Div

@cache_definition
def DefineLt_Atom(has_valid: bool = False):
    class _Lt(Circuit):
        name = "Lt_Atom"
        IO = ['I', In(ST_Atom_Tuple(ST_Int(), ST_Int()).magma_repr()),
              'O', Out(ST_Bit().magma_repr())]
        if has_valid:
            IO += valid_ports
        binary_op = False
        st_in_t = [ST_Atom_Tuple(ST_Int(), ST_Int())]
        st_out_t = ST_Bit()
        @classmethod
        def definition(cls):
            op = DefineCoreirUlt(int_width)()
            wire(cls.I[0], op.I0)
            wire(cls.I[1], op.I1)
            wire(op.O, cls.O[0])
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _Lt

@cache_definition
def DefineEq_Atom(t: ST_Type, has_valid: bool = False):
    class _Eq(Circuit):
        name = "Eq_Atom_{}t".format(cleanName(str(t)))
        IO = ['I', In(ST_Atom_Tuple(t, t).magma_repr()),
              'O', Out(ST_Bit().magma_repr())]
        if has_valid:
            IO += valid_ports
        binary_op = False
        st_in_t = [ST_Atom_Tuple(ST_Int(), ST_Int())]
        st_out_t = ST_Bit()
        @classmethod
        def definition(cls):
            op = DefineEQ(t.magma_repr().size())()
            wire(cls.I[0], op.I0)
            wire(cls.I[1], op.I1)
            wire(op.O, cls.O[0])
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _Eq

@cache_definition
def DefineIf_Atom(t: ST_Type, has_valid: bool = False):
    class _If(Circuit):
        name = "If_Atom_{}t".format(cleanName(str(t)))
        binary_op = False
        st_in_t = [ST_Atom_Tuple(ST_Bit(), ST_Atom_Tuple(t, t))]
        st_out_t = t
        IO = ['I', In(st_in_t[0].magma_repr()),
              'O', Out(st_out_t.magma_repr())]
        if has_valid:
            IO += valid_ports
        @classmethod
        def definition(cls):
            op = DefineMuxAnyType(t.magma_repr(), 2)()
            wire(cls.I[0], op.sel)
            wire(cls.I[1][0], op.data[1])
            wire(cls.I[1][1], op.data[0])
            wire(op.out, cls.O)
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _If
