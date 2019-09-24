from aetherling.modules.counter import AESizedCounterModM
from aetherling.modules.initial_delay_counter import DefineInitialDelayCounter
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

def atom_or_sseq_to_bits(atom):
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


def ts_arrays_to_bits(ts_xss):
    """
    Convert a list of sseqs of atoms to a tuple of tuple of bits.
    Each inner tuple of bits is one clock cycle
    """
    ts_flattened = []
    for s_xss in ts_xss:
        ts_flattened.append(builtins.tuple(atom_or_sseq_to_bits(s_xss)))
    return builtins.tuple(ts_flattened)




