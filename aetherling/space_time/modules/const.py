from aetherling.modules.counter import AESizedCounterModM
from aetherling.modules.initial_delay_counter import DefineInitialDelayCounter
from mantle.coreir import DefineCoreirConst
from magma import *
from magma.bitutils import int2seq
from magma.circuit import DefineCircuitKind
from aetherling.helpers.nameCleanup import cleanName
from aetherling.space_time.space_time_types import *
from aetherling.space_time.type_helpers import valid_ports
from aetherling.modules.ram_any_type import DefineRAMAnyType

def atom_to_bits(atom):
    if type(atom) == int:
        return int2seq(atom, int_width)
    elif type(atom) == bool:
        if atom == True:
            return [1]
        else:
            return [0]
    elif type(atom) == list:
        concat()



