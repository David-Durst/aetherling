from aetherling.helpers.nameCleanup import cleanName
from magma import *
from magma.backend.coreir_ import CoreIRBackend
from aetherling.modules.hydrate import Dehydrate, Hydrate
from mantle.coreir.memory import DefineRAM, getRAMAddrWidth


__all__ = ['DefineDelayedBuffer', 'DelayedBuffer']

@cache_definition
def DefineDelayedBuffer(cirb: CoreIRBackend, t: Kind, n: int, total_emitting_period):
    """
    Generate a buffer that accepts n items, emtting them evenly spaced over a total_emitting_period
     number of clocks.

    I: In(t), O: Out(t), WE: In(Bit), CE: In(Enable)
    """

    class _RAM(Circuit):
        name = 'RAM_{}t'.format(cleanName(str(t)))
        addr_width = getRAMAddrWidth(n)
        IO = ['I', In(t),
              'O', Out(t),
              'WE', In(Bit)
             ] + ClockInterface(has_ce=True)
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

def DelayedBuffer(cirb: CoreIRBackend, t: Kind, n: int):
    return DefineRAMAnyType(cirb, t, n)()
