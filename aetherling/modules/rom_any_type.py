from aetherling.helpers.nameCleanup import cleanName
from magma import *
from magma.frontend.coreir_ import GetCoreIRBackend
from aetherling.modules.hydrate import Dehydrate, Hydrate
from mantle.coreir.memory import DefineROM, getRAMAddrWidth
import typing

__all__ = ['DefineROMAnyType', 'ROMAnyType']

@cache_definition
def DefineROMAnyType(t: Kind, n: int, init: typing.Tuple):
    """
    DO NOT USE. CAN'T PASS JSON ARGUMENTS YET
    """

    class _ROM(Circuit):
        name = 'ROM_{}t_{}n'.format(cleanName(str(t)), n)
        addr_width = getRAMAddrWidth(n)
        IO = ['RADDR', In(Bits[addr_width]),
              'RDATA', Out(t),
              'REN', In(Bit)
              ] + ClockInterface()
        @classmethod
        def definition(cls):
            type_size_in_bits = GetCoreIRBackend().get_type(t).size
            rom = DefineROM(n, type_size_in_bits)(coreir_configargs={"init": [list(el) for el in init]})

            bits_to_type = Hydrate(t)
            wire(rom.rdata, bits_to_type.I)
            wire(bits_to_type.out, cls.RDATA)

            wire(cls.RADDR, rom.raddr)

            wire(cls.REN, rom.ren)

    return _ROM

def ROMAnyType(t: Kind, n: int, init: typing.Tuple):
    return DefineROMAnyType(t, n, init)()
