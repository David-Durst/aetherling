from aetherling.space_time import *
from aetherling.space_time.type_helpers import valid_ports
from mantle import Decode
from mantle.coreir import DefineCoreirConst
from magma import *
from magma.circuit import DefineCircuitKind
from aetherling.helpers.nameCleanup import cleanName
from aetherling.modules.register_any_type import DefineRegisterAnyType
from aetherling.modules.mux_any_type import DefineMuxAnyType
from aetherling.modules.term_any_type import TermAnyType
from aetherling.modules.ram_any_type import DefineRAMAnyType
from aetherling.modules.counter import AESizedCounterModM

__all__ = ['DefineDown_S', 'Down_S']

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
    class _Up_S(Circuit):
        name = "Down_S_n{}_sel{}_tEl{}_v{}".format(str(n), str(idx), cleanName(str(elem_t)), str(has_valid))
        IO = ['I', In(ST_SSeq(n, elem_t).magma_repr()), 'O', Out(ST_SSeq(1, elem_t).magma_repr())] + \
             ClockInterface(False, False)
        if has_valid:
            IO += valid_ports
        @classmethod
        def definition(cls):
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
    return _Up_S

def Down_S(n: int, idx: int, elem_t: ST_Type, has_valid: bool = False) -> Circuit:
    return DefineDown_S(n, idx, elem_t, has_valid)()

