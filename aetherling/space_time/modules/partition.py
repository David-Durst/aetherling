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

__all__ = ['DefinePartition_S', 'Partition_S', 'DefineUnpartition_S', 'Unpartition_S']

@cache_definition
def DefinePartition_S(no: int, ni: int, elem_t: ST_Type, has_valid: bool = False) -> DefineCircuitKind:
    """
    Convert a SSeq (no*ni) elem_t to SSeq no (SSeq ni elem_t)

    I : In(SSeq(no*ni, elem_t).magma_repr())
    O : Out(SSeq(no, SSeq(ni, elem_t)).magma_repr())
    if has_valid:
    valid_up : In(Bit)
    valid_down : Out(Bit)
    """
    class _Up_S(Circuit):
        name = "Partition_S_no{}_ni{}_tEl{}_v{}".format(str(no), str(ni), cleanName(str(elem_t)), str(has_valid))
        IO = ['I', In(ST_SSeq(no*ni, elem_t).magma_repr()),
              'O', Out(ST_SSeq(no, ST_SSeq(ni, elem_t)).magma_repr())] + \
             ClockInterface(False, False)
        if has_valid:
            IO += valid_ports
        @classmethod
        def definition(cls):
            for i in range(no):
                for j in range(ni):
                    wire(cls.I[i*ni + j], cls.O[i][j])
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _Up_S

def Partition_S(no: int, ni: int, elem_t: ST_Type, has_valid: bool = False) -> Circuit:
    return DefineDown_S(no, ni, elem_t, has_valid)()

@cache_definition
def DefineUnpartition_S(no: int, ni: int, elem_t: ST_Type, has_valid: bool = False) -> DefineCircuitKind:
    """
    Convert a SSeq no (SSeq ni elem_t) to SSeq (no*ni) elem_t

    I : In(SSeq(no, SSeq(ni, elem_t)).magma_repr())
    O : In(SSeq(no*ni, elem_t).magma_repr())
    if has_valid:
    valid_up : In(Bit)
    valid_down : Out(Bit)
    """
    class _Up_S(Circuit):
        name = "Partition_S_no{}_ni{}_tEl{}_v{}".format(str(no), str(ni), cleanName(str(elem_t)), str(has_valid))
        IO = ['I', In(ST_SSeq(no, ST_SSeq(ni, elem_t)).magma_repr()),
              'O', Out(ST_SSeq(no * ni, elem_t).magma_repr())] + \
             ClockInterface(False, False)
        if has_valid:
            IO += valid_ports
        @classmethod
        def definition(cls):
            for i in range(no):
                for j in range(ni):
                    wire(cls.O[i*ni + j], cls.I[i][j])
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _Up_S

def Unpartition_S(no: int, ni: int, elem_t: ST_Type, has_valid: bool = False) -> Circuit:
    return DefineDown_S(no, ni, elem_t, has_valid)()
