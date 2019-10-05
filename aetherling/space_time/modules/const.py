from aetherling.modules.counter import AESizedCounterModM
from aetherling.modules.lut_any_type import DefineLUTAnyType
from aetherling.modules.term_any_type import TermAnyType
from mantle.coreir import DefineCoreirConst
from magma import *
from magma.bitutils import int2seq
from magma.circuit import DefineCircuitKind
from aetherling.helpers.nameCleanup import cleanName
from aetherling.space_time.space_time_types import *
from aetherling.space_time.type_helpers import valid_ports, flatten as ae_flatten
from aetherling.modules.ram_any_type import DefineRAMAnyType
from collections.abc import Iterable
import builtins
from typing import Union, Tuple

def atom_or_sseq_to_bits(atom: Union[int, bool, Tuple]) -> Tuple:
    """
    This converts atoms or SSeqs of atoms to flat bits
    :return:
    """
    if type(atom) == int:
        return builtins.tuple(int2seq(atom, int_width))
    elif type(atom) == bool:
        if atom == True:
            return builtins.tuple([1])
        else:
            return builtins.tuple([0])
    elif isinstance(atom, Iterable):
        return builtins.tuple(ae_flatten([atom_or_sseq_to_bits(subatom) for subatom in atom]))
    else:
        raise NotImplementedError("Type {} not supported".format(str(type(atom))))


def ts_arrays_to_bits(ts_xss: Tuple) -> Tuple:
    """
    Convert a list of sseqs of atoms to a tuple of tuple of bits.
    Each inner tuple of bits is one clock cycle
    Note: will be a list of atoms if each sseq is length 1. Should also work.
    """
    ts_flattened = []
    for s_xss in ts_xss:
        ts_flattened.append(builtins.tuple(atom_or_sseq_to_bits(s_xss)))
    return builtins.tuple(ts_flattened)


@cache_definition
def DefineConst(t: ST_Type, ts_values: Tuple,
               has_ce: bool = False, has_reset: bool = False, has_valid: bool = False) -> DefineCircuitKind:
    """
    Emits a constant over multiple clocks

    O : Out(t)

    if has_ce:
    CE : In(Bit)
    if has_reset:
    RESET : In(Bit)
    if has_valid:
    valid_down : Out(Bit)
    """
    class _Const(Circuit):
        name = "Const_t{}_hasCE{}_hasReset{}_hasValid{}".format(cleanName(str(t)), str(has_ce),
                                                                str(has_reset), str(has_valid))
        IO = ['O', Out(t.magma_repr())] + ClockInterface(has_ce, has_reset)
        binary_op = False
        st_in_t = []
        st_out_t = t
        if has_valid:
            IO += valid_ports
        @classmethod
        def definition(cls):
            enabled = DefineCoreirConst(1, 1)().O[0]
            if has_ce:
                enabled = bit(cls.CE) & enabled

            luts = DefineLUTAnyType(t.magma_repr(), t.time(), ts_arrays_to_bits(ts_values))()
            lut_position_counter = AESizedCounterModM(t.time(), has_ce=True, has_reset=has_reset)

            wire(lut_position_counter.O, luts.addr)
            wire(cls.O, luts.data)
            wire(enabled, lut_position_counter.CE)

            if has_reset:
                wire(cls.RESET, lut_position_counter.RESET)
            if has_valid:
                valid_up_term = TermAnyType(Bit)
                wire(cls.valid_up, valid_up_term.I)
                wire(enabled, cls.valid_down)


    return _Const

