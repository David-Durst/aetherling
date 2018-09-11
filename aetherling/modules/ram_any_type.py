from aetherling.helpers.nameCleanup import cleanName
from magma import *
from magma.backend.coreir_ import CoreIRBackend
from aetherling.modules.hydrate import Dehydrate, Hydrate
from mantle.coreir.memory import DefineRAM, getRAMAddrWidth


__all__ = ['DefineRAMAnyType', 'RAMAnyType']

@cache_definition
def DefineRAMAnyType(cirb: CoreIRBackend, t: Kind, n: int):
    """
    Generate a RAM that handles n of any type.

    RADDR : In(Array(n, Bit)), RDATA : Out(t), WADDR : In(Array(n, Bit)), WDATA : In(t), WE: In(Bit)
    """

    class _RAM(Circuit):
        name = 'RAM_{}t'.format(cleanName(str(t)))
        addr_width = getRAMAddrWidth(n)
        IO = ['RADDR', In(Bits(addr_width)),
              'RDATA', Out(t),
              'WADDR', In(Bits(addr_width)),
              'WDATA', In(t),
              'WE', In(Bit)
             ]
        @classmethod
        def definition(cls):
            type_size_in_bits = cirb.get_type(t).size
            ram = DefineRAM(n, type_size_in_bits)()

            type_to_bits = Dehydrate(cirb, t)
            wire(cls.WDATA, type_to_bits.I)
            wire(type_to_bits.out, ram.WDATA)

            bits_to_type = Hydrate(cirb, t)
            wire(ram.RDATA, bits_to_type.I)
            wire(bits_to_type.out, cls.RDATA)

            wire(cls.RADDR, ram.RADDR)
            wire(ram.WADDR, cls.WADDR)

            wire(cls.WE, ram.WE)

    return _RAM

def RAMAnyType(cirb: CoreIRBackend, t: Kind, n: int):
    return DefineRAMAnyType(cirb, t, n)()
