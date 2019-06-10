from aetherling.helpers.nameCleanup import cleanName
from magma import *
from magma.frontend.coreir_ import GetCoreIRBackend
from mantle.common.sipo import DefineSIPO
from mantle.common.register import _RegisterName
from aetherling.modules.hydrate import Dehydrate, DefineHydrate
from aetherling.modules.map_fully_parallel_sequential import MapParallel


__all__ = ['DefineSIPOAnyType', 'SIPOAnyType']

@cache_definition
def DefineSIPOAnyType(n: int, t: Kind, init: int = 0,
                      has_ce: bool = False, has_reset: bool = False):
    """
    Generate Serial-In, Parallel-Out shift register that handles any type.

    I : In(t),  O : Out(Array[n, t])
    """

    class _SIPO(Circuit):
        name = 'SIPO_{}t_{}n_{}init_{}CE_RESET'.format(
            cleanName(str(t)), str(n), str(init), str(has_ce), str(has_reset))
        IO = ['I', In(t), 'O', Out(Array[n, t])] + \
                ClockInterface(has_ce,has_reset)
        @classmethod
        def definition(cls):
            type_size_in_bits = GetCoreIRBackend().get_type(t).size
            type_to_bits = Dehydrate(t)
            sipos = MapParallel(type_size_in_bits, DefineSIPO(n, init, has_ce, has_reset))
            bits_to_type = MapParallel(n, DefineHydrate(t))

            for bit_in_type in range(type_size_in_bits):
                wire(type_to_bits.out[bit_in_type], sipos.I[bit_in_type])
                for sipo_output in range(n):
                    wire(sipos.O[bit_in_type][sipo_output], bits_to_type.I[sipo_output][bit_in_type])

            wire(cls.I, type_to_bits.I)
            wire(bits_to_type.out, cls.O)

            for bit_in_type in range(type_size_in_bits):
                if has_ce:
                    wire(cls.CE, sipos.CE[bit_in_type])
                if has_reset:
                    wire(cls.RESET, sipos.RESET[bit_in_type])

    return _SIPO

def SIPOAnyType(n: int, t: Kind, init: int = 0,
                      has_ce: bool = False, has_reset: bool = False):
    return DefineSIPOAnyType(n, t, init, has_ce, has_reset)()
