from aetherling.helpers.nameCleanup import cleanName
from magma import *
from magma.frontend.coreir_ import GetCoreIRBackend
from mantle.common.register import DefineRegister
from aetherling.modules.hydrate import Dehydrate, Hydrate
from aetherling.modules.map_fully_parallel_sequential import MapParallel


__all__ = ['DefineRegisterAnyType', 'RegisterAnyType']

@cache_definition
def DefineRegisterAnyType(t: Kind, init: int = 0, has_ce: bool = False, has_reset: bool = False):
    """
    Generate register that handles any type.

    I : In(t),  O : Out(t)

    If set:

    CE : In(Bit), RESET : In(Bit)
    """

    class _Register(Circuit):
        name = 'Register_{}t_{}init_{}CE_{}RESET'.format(
            cleanName(str(t)), str(init), str(has_ce), str(has_reset))
        IO = ['I', In(t), 'O', Out(t)] + \
                ClockInterface(has_ce,has_reset)
        @classmethod
        def definition(cls):
            type_size_in_bits = GetCoreIRBackend().get_type(t).size
            type_to_bits = Dehydrate(t)
            registers = DefineRegister(type_size_in_bits, has_ce=has_ce, has_reset=has_reset)()
            bits_to_type = Hydrate(t)

            #for bit_in_type in range(type_size_in_bits):
            #    wire(type_to_bits.out[bit_in_type], sipos.I[bit_in_type])
            #    for sipo_output in range(n):
            #        wire(sipos.O[bit_in_type][sipo_output], bits_to_type.I[sipo_output][bit_in_type])

            wire(cls.I, type_to_bits.I)
            wire(type_to_bits.out, registers.I)
            wire(registers.O, bits_to_type.I)
            wire(bits_to_type.out, cls.O)

            if has_ce:
                wire(cls.CE, registers.CE)
            if has_reset:
                wire(cls.RESET, registers.RESET)
            #for bit_in_type in range(type_size_in_bits):
            #    if has_ce:
            #        wire(cls.CE, sipos.CE[bit_in_type])
            #    if has_reset:
            #        wire(cls.RESET, sipos.RESET[bit_in_type])

    return _Register

def RegisterAnyType(t: Kind, init: int = 0, has_ce: bool = False, has_reset: bool = False):
    return DefineRegisterAnyType(t, init, has_ce, has_reset)()


