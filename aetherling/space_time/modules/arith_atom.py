from mantle.coreir.arith import *
from mantle.coreir.logic import *
from mantle.coreir.compare import *
from mantle.coreir import DefineCoreirConst, DefineTerm
from aetherling.space_time.space_time_types import *
from aetherling.space_time.type_helpers import valid_ports
from aetherling.modules.mux_any_type import DefineMuxAnyType
from aetherling.helpers.nameCleanup import cleanName
from mantle.common.register import DefineRegister
from magma import *
import os

int_width = ST_Int().magma_repr().size()
bit_width = ST_Bit().magma_repr().size()

@cache_definition
def DefineAbs_Atom(has_valid: bool = False, t: ST_Type = ST_Int()):
    class _Abs(Circuit):
        name = "Abs_Atom"
        IO = ['I', In(t.magma_repr()), 'O', Out(t.magma_repr())]
        if has_valid:
            IO += valid_ports
        binary_op = False
        st_in_t = [t]
        st_out_t = t
        @classmethod
        def definition(cls):
            neg = DefineNegate(t.length())()
            cmp = DefineCoreirUgt(t.length())()
            mux = DefineMuxAnyType(t.magma_repr(), 2)()
            wire(cls.I, mux.data[0])
            wire(cls.I, neg.I)
            wire(neg.O, mux.data[1])
            wire(cls.I, cmp.I0)
            wire(neg.O, cmp.I1)
            wire(cmp.O, mux.sel[0])
            abs_reg = DefineRegister(cls.st_out_t.magma_repr().N)()
            wire(mux.out, abs_reg.I)
            wire(abs_reg.O, cls.O)

            if has_valid:
                valid_reg = DefineRegister(1)()
                wire(cls.valid_up, valid_reg.I[0])
                wire(valid_reg.O[0], cls.valid_down)
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
def DefineAnd_Atom(has_valid: bool = False):
    class _And(Circuit):
        name = "And_Atom"
        IO = ['I', In(ST_Atom_Tuple(ST_Bit(), ST_Bit()).magma_repr()), 'O', Out(ST_Bit().magma_repr())]
        if has_valid:
            IO += valid_ports
        binary_op = False
        st_in_t = [ST_Atom_Tuple(ST_Int(), ST_Int())]
        st_out_t = ST_Bit()
        @classmethod
        def definition(cls):
            op = DefineAnd(width=bit_width)()
            wire(cls.I[0], op.I0)
            wire(cls.I[1], op.I1)
            wire(op.O, cls.O)
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _And

@cache_definition
def DefineOr_Atom(has_valid: bool = False):
    class _Or(Circuit):
        name = "Or_Atom"
        IO = ['I', In(ST_Atom_Tuple(ST_Bit(), ST_Bit()).magma_repr()), 'O', Out(ST_Bit().magma_repr())]
        if has_valid:
            IO += valid_ports
        binary_op = False
        st_in_t = [ST_Atom_Tuple(ST_Int(), ST_Int())]
        st_out_t = ST_Bit()
        @classmethod
        def definition(cls):
            op = DefineOr(width=bit_width)()
            wire(cls.I[0], op.I0)
            wire(cls.I[1], op.I1)
            wire(op.O, cls.O)
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _Or

@cache_definition
def DefineAdd_Atom(has_valid: bool = False, t: ST_Type = ST_Int()):
    class _Add(Circuit):
        name = "Add_Atom"
        IO = ['I', In(ST_Atom_Tuple(t, t).magma_repr()),
              'O', Out(t.magma_repr())]
        if has_valid:
            IO += valid_ports
        binary_op = False
        st_in_t = [ST_Atom_Tuple(t, t)]
        st_out_t = t
        @classmethod
        def definition(cls):
            op = DefineAdd(t.length())()
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
            dir_path = os.path.dirname(os.path.realpath(__file__))
            op = m.DefineFromVerilogFile(os.path.join(dir_path, "pipelined", "mul.v"), type_map={"CLK": m.In(m.Clock)})[0]()
            zero_const = DefineCoreirConst(1, 0)()
            one_const = DefineCoreirConst(1, 1)()
            wire(zero_const.O[0], op.rst)
            wire(one_const.O[0], op.ce)
            wire(cls.I[0], op.a)
            wire(cls.I[1], op.b)
            wire(op.p[0:8], cls.O)
            term = DefineTerm(8)()
            wire(op.p[8:16], term.I)
            if has_valid:
                reg0 = DefineRegister(1)()
                reg1 = DefineRegister(1)()
                wire(cls.valid_up, reg0.I[0])
                wire(reg0.O, reg1.I)
                wire(reg1.O[0], cls.valid_down)
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
def DefineRShift_Atom(has_valid: bool = False):
    class _Mul(Circuit):
        name = "RShift_Atom"
        IO = ['I', In(ST_Atom_Tuple(ST_Int(), ST_Int()).magma_repr()),
              'O', Out(ST_Int().magma_repr())]
        if has_valid:
            IO += valid_ports
        binary_op = False
        st_in_t = [ST_Atom_Tuple(ST_Int(), ST_Int())]
        st_out_t = ST_Int()
        @classmethod
        def definition(cls):
            op = DefineLSR(int_width)()
            wire(cls.I[0], op.I0)
            wire(cls.I[1], op.I1)
            wire(op.O, cls.O)
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _Mul

@cache_definition
def DefineLShift_Atom(has_valid: bool = False):
    class _Mul(Circuit):
        name = "LShift_Atom"
        IO = ['I', In(ST_Atom_Tuple(ST_Int(), ST_Int()).magma_repr()),
              'O', Out(ST_Int().magma_repr())]
        if has_valid:
            IO += valid_ports
        binary_op = False
        st_in_t = [ST_Atom_Tuple(ST_Int(), ST_Int())]
        st_out_t = ST_Int()
        @classmethod
        def definition(cls):
            op = DefineLSL(int_width)()
            wire(cls.I[0], op.I0)
            wire(cls.I[1], op.I1)
            wire(op.O, cls.O)
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _Mul

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
