from aetherling.space_time import *
from aetherling.space_time.type_helpers import valid_ports
from mantle import Decode
from mantle.coreir import DefineCoreirConst
from magma import *
from aetherling.helpers.nameCleanup import cleanName
from aetherling.modules.register_any_type import DefineRegisterAnyType
from aetherling.modules.mux_any_type import DefineMuxAnyType
from aetherling.modules.term_any_type import TermAnyType
from aetherling.modules.ram_any_type import DefineRAMAnyType
from aetherling.modules.counter import AESizedCounterModM

__all__ = ['DefineUp_S', 'Up_S', 'DefineUp_T', 'Up_T']

@cache_definition
def DefineUp_S(n: int, elem_t: ST_Type, has_valid: bool = False) -> DefineCircuitKind:
    """
    Convert a SSeq 1 elem_t to SSeq n elem_t

    I : In(T)
    O : Out(Array[n, T])
    if has_valid:
    valid_up : In(Bit)
    valid_down : Out(Bit)
    """
    class _Up_S(Circuit):
        name = "Up_S_n{}_tEl{}_v{}".format(str(n), cleanName(str(elem_t)), str(has_valid))
        IO = ['I', In(ST_SSeq(1, elem_t).magma_repr()), 'O', Out(ST_SSeq(n, elem_t).magma_repr())]
        if has_valid:
            IO += valid_ports
        @classmethod
        def definition(cls):
            for i in range(n):
                cls.wire = wire(cls.I, cls.O[i])
            if has_valid:
                wire(cls.ready_up, cls.ready_down)
                wire(cls.valid_up, cls.valid_down)
    return _Up_S

def Up_S(n: int, elem_t: ST_Type, has_valid: bool = False) -> Circuit:
    return DefineUp_S(n, elem_t, has_valid)()

@cache_definition
def DefineUp_T(n: int, i: int, elem_t: ST_Type,
               has_ce: bool = False, has_reset: bool = False, has_valid: bool = False) -> DefineCircuitKind:
    """
    Convert a TSeq 1 (n-1) elem_t to TSeq n 0 elem_t

    I : In(T)
    O : Out(Array[n, T])
    if has_ce:
    CE : In(Bit)
    if has_reset:
    RESET : In(Bit)
    if has_valid:
    valid_up : In(Bit)
    valid_down : Out(Bit)
    """
    class _Up_T(Circuit):
        name = "Up_T_n{}_i{}_tEl{}__hasCE{}_hasReset{}_hasValid{}".format(str(n), str(i),
                                                                          cleanName(str(elem_t)),
                                                                          str(has_ce), str(has_reset),
                                                                          str(has_valid))
        IO = ['I', In(T), 'O', Out(T)] + ClockInterface(has_ce, has_reset)
        if has_valid:
            IO += valid_ports
        @classmethod
        def definition(cls):
            enabled = DefineCoreirConst(1, 1)().O[0]
            if has_valid:
                enabled = cls.valid_up & enabled
                wire(cls.valid_up, cls.valid_down)
            if has_ce:
                enabled = bit(cls.CE) & enabled

            value_store = DefineRAM_ST(elem_t, 1, has_reset=has_reset)()

            # write to value_store for first element, read for next
            element_time_counter = DefineNestedCounters(elem_t, has_ce=True, has_reset=has_reset)()
            element_idx_counter = AESizedCounterModM(n, has_ce=True, has_reset=has_reset)
            is_first_element = Decode(0, element_idx_counter.O.N)(element_idx_counter.O)

            zero_addr = DefineCoreirConst(1, 0)().O[0]
            wire(zero_addr, value_store.WADDR)
            wire(zero_addr, value_store.RADDR)

            wire(enabled & is_first_element, value_store.WE)
            wire(enabled, value_store.RE)
            wire(enabled, element_time_counter.CE)
            wire(enabled & element_time_counter.last, element_idx_counter.CE)

            element_time_counter_term = TermAnyType(Bit)
            wire(element_time_counter.valid, element_time_counter_term.I)

            wire(cls.I, value_store.WDATA)

            output_selector = DefineMuxAnyType(elem_t.magma_repr(), 2)()

            # on first element, send the input directly out. otherwise, use the register
            wire(is_first_element, output_selector.sel[0])
            wire(value_store.RDATA, output_selector.data[0])
            wire(cls.I, output_selector.data[1])
            wire(output_selector.out, cls.O)

            if has_reset:
                wire(value_store.RESET, cls.RESET)
                wire(element_time_counter.RESET, cls.RESET)
                wire(element_idx_counter.RESET, cls.RESET)

    return _Up_T

def Up_T(n: int, i: int, elem_t: ST_Type, has_valid: bool = False) -> Circuit:
    return DefineUp_T(n, i, elem_t, has_valid)()

