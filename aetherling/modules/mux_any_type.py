from aetherling.helpers.nameCleanup import cleanName
from aetherling.modules.term_any_type import DefineTermAnyType
from magma import *
from magma.backend.coreir_ import CoreIRBackend
from aetherling.modules.hydrate import DefineDehydrate, Hydrate
from aetherling.modules.map_fully_parallel_sequential import MapParallel
from mantle.coreir.memory import getRAMAddrWidth
from mantle.coreir.MUX import CommonlibMuxN


__all__ = ['DefineMuxAnyType', 'MuxAnyType']

@cache_definition
def DefineMuxAnyType(cirb: CoreIRBackend, t: Kind, n: int):
    """
    Generate a Mux that handles n of any type.

    data : In(Array(n, t)), sel : In(Array(log_2(n), Bit)), out : Out(t)
    """

    class _Mux(Circuit):
        name = 'Mux_{}t_{}n'.format(cleanName(str(t)), n)
        addr_width = getRAMAddrWidth(n)
        IO = ['data', In(Array(n, t)),
              'sel', In(Bits(addr_width)),
              'out', Out(t)
             ]
        @classmethod
        def definition(cls):
            if n > 1:
                type_size_in_bits = cirb.get_type(t).size
                mux = CommonlibMuxN(cirb, n, type_size_in_bits)

                type_to_bits = MapParallel(cirb, n, DefineDehydrate(cirb, t))
                wire(cls.data, type_to_bits.I)
                wire(type_to_bits.out, mux.I.data)

                bits_to_type = Hydrate(cirb, t)
                wire(mux.out, bits_to_type.I)
                wire(bits_to_type.out, cls.out)

                wire(cls.sel, mux.I.sel)
            else:
                wire(cls.data[0], cls.out)
                sel_term = DefineTermAnyType(cirb, Bits(cls.addr_width))()
                wire(cls.sel, sel_term.I)

    return _Mux

def MuxAnyType(cirb: CoreIRBackend, t: Kind, n: int):
    return DefineMuxAnyType(cirb, t, n)()
