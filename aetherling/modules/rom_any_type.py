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
    Generate a RAM that handles n of any type. Note that each value in init must already have been converted into bits.

    RADDR : In(Array[log_2(n), Bit)], RDATA : Out(t), WADDR : In(Array(log_2(n), Bit)), WDATA : In(t), WE: In(Bit)
    """

    class _ROM(Circuit):
        name = 'ROM_{}t_{}n'.format(cleanName(str(t)), n)
        addr_width = getRAMAddrWidth(n)
        IO = ['RADDR', In(Bits[addr_width]),
              'RDATA', Out(t),
              'RE', In(Bit)
              ] + ClockInterface()
        @classmethod
        def definition(cls):
            type_size_in_bits = GetCoreIRBackend().get_type(t).size
            rom = DefineROM(n, type_size_in_bits, [list(el) for el in init])()

            bits_to_type = Hydrate(t)
            wire(rom.rdata, bits_to_type.I)
            wire(bits_to_type.out, cls.RDATA)

            wire(cls.RADDR, rom.raddr)

            wire(cls.RE, rom.re)

    return _ROM

def ROMAnyType(t: Kind, n: int, init: typing.Tuple):
    return DefineROMAnyType(t, n, init)()
