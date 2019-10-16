from aetherling.space_time import *
from aetherling.space_time.type_helpers import valid_ports
from aetherling.modules.initial_delay_counter import InitialDelayCounter
from magma import *
from magma.circuit import DefineCircuitKind
from aetherling.helpers.nameCleanup import cleanName
from aetherling.modules.term_any_type import TermAnyType

__all__ = ['DefineDown_S', 'Down_S', 'DefineDown_T' ,'Down_T']

@cache_definition
def DefineDown_S(n: int, idx: int, elem_t: ST_Type, has_valid: bool = False) -> DefineCircuitKind:
    """
    Convert a SSeq n elem_t to SSeq 1 elem_t

    I : In(SSeq(n, elem_t).magma_repr())
    O : Out(SSeq(1, elem_t).magma_repr())
    if has_valid:
    valid_up : In(Bit)
    valid_down : Out(Bit)
    """
    class _Down_S(Circuit):
        name = "Down_S_n{}_sel{}_tEl{}_v{}".format(str(n), str(idx), cleanName(str(elem_t)), str(has_valid))
        binary_op = False
        st_in_t = [ST_SSeq(n, elem_t)]
        st_out_t = ST_SSeq(1, elem_t)
        IO = ['I', In(st_in_t[0].magma_repr()), 'O', Out(st_out_t.magma_repr())] + \
             ClockInterface(False, False)
        if has_valid:
            IO += valid_ports
        @classmethod
        def definition(cls):
            if n > 1:
                inputs_term = TermAnyType(ST_SSeq(n-1, elem_t).magma_repr())
            num_wired_to_output = 0
            for i in range(len(cls.I)):
                if i == idx:
                    wire(cls.I[i], cls.O[0])
                    num_wired_to_output += 1
                else:
                    wire(cls.I[i], inputs_term.I[i - num_wired_to_output])
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _Down_S

def Down_S(n: int, idx: int, elem_t: ST_Type, has_valid: bool = False) -> Circuit:
    return DefineDown_S(n, idx, elem_t, has_valid)()

@cache_definition
def DefineDown_T(n: int, i:int, idx: int, elem_t: ST_Type, has_valid: bool = False) -> DefineCircuitKind:
    """
    Convert a TSeq n i elem_t to TSeq 1 (n-1+i) elem_t

    I : In(TSeq(n, i, elem_t).magma_repr())
    O : Out(SSeq(1, n-1+i, elem_t).magma_repr())
    if has_valid:
    valid_up : In(Bit)
    valid_down : Out(Bit)
    """
    class _Down_T(Circuit):
        name = "Down_S_n{}_i{}_sel{}_tEl{}_v{}".format(str(n), str(i), str(idx), cleanName(str(elem_t)), str(has_valid))
        binary_op = False
        st_in_t = [ST_TSeq(n, i, elem_t)]
        st_out_t = ST_TSeq(1, n+i-1, elem_t)
        IO = ['I', In(st_in_t[0].magma_repr()), 'O', Out(st_out_t.magma_repr())] + \
             ClockInterface(False, False)
        if has_valid:
            IO += valid_ports
        @classmethod
        def definition(cls):
            wire(cls.I, cls.O)
            if has_valid:
                if elem_t.time()*idx == 0:
                    wire(cls.valid_up, cls.valid_down)
                else:
                    delay_counter = InitialDelayCounter(elem_t.time()*idx)
                    wire(cls.valid_up, delay_counter.CE)
                    wire(delay_counter.valid, cls.valid_down)
    return _Down_T

def Down_T(n: int, i: int, idx: int, elem_t: ST_Type, has_valid: bool = False) -> Circuit:
    return DefineDown_T(n, i, idx, elem_t, has_valid)()

