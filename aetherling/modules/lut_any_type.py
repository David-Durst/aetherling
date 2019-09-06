from aetherling.helpers.nameCleanup import cleanName
from magma import *
from magma.bitutils import *
from magma.frontend.coreir_ import GetCoreIRBackend
from aetherling.modules.hydrate import Dehydrate, Hydrate
from mantle.coreir.LUT import DefineLUT
from mantle.coreir.memory import getRAMAddrWidth
import typing

__all__ = ['DefineLUTAnyType', 'LUTAnyType']

@cache_definition
def DefineLUTAnyType(t: Kind, n: int, init: typing.Tuple):
    """
    Generate a LUT that handles n of any type. Note that each value in init must already have been converted into bits.

    addr : In(Array[log_2(n), Bit)], data : Out(t)
    """

    class _ROM(Circuit):
        name = 'LUT_{}t_{}n'.format(cleanName(str(t)), n)
        addr_width = getRAMAddrWidth(n)
        IO = ['addr', In(Bits[addr_width]),
              'data', Out(t),
              ] + ClockInterface()
        @classmethod
        def definition(cls):
            type_size_in_bits = GetCoreIRBackend().get_type(t).size
            bit_luts = []
            for i in range(type_size_in_bits):
                bit_luts += [DefineLUT(seq2int([el[i] for el in init]), getRAMAddrWidth(n))()]

            bits_to_type = Hydrate(t)
            for i in range(type_size_in_bits):
                wire(bit_luts[i].O, bits_to_type.I[i])
            wire(bits_to_type.out, cls.data)

            for i in range(type_size_in_bits):
                for j in range(cls.addr_width):
                    wire(cls.addr[j], getattr(bit_luts[i], "I" + str(j)))

    return _ROM

def LUTAnyType(t: Kind, n: int, init: typing.Tuple):
    return DefineROMAnyType(t, n, init)()
