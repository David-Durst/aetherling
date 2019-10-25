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
            # if using a layer of nesting
            nested = False
            if issubclass(type(t), ArrayKind):
                if type(t.T) == BitKind:
                    regs = [DefineRegister(t.N, has_ce=has_ce, has_reset=has_reset)()]
                    nested = True
                else:
                    regs = [DefineRegisterAnyType(t.T, init, has_ce, has_reset)() for _ in range(t.N)]
            elif issubclass(type(t), TupleKind):
                regs = [DefineRegisterAnyType(t_inner, init, has_ce, has_reset)() for t_inner in t.Ts]
            else:
                regs = [DefineRegister(1, init, has_ce, has_reset)()]
            if nested:
                for i in range(t.N):
                    wire(cls.I[i], regs[0].I[i])
                    wire(cls.O[i], regs[0].O[i])
                if has_ce:
                    wire(cls.CE, regs[0].CE)
                if has_reset:
                    wire(cls.RESET, regs[0].RESET)
            else:
                for i,reg in enumerate(regs):
                    if type(t) == BitKind:
                        wire(cls.I, reg.I[0])
                        wire(cls.O, reg.O[0])
                    else:
                        wire(cls.I[i], reg.I)
                        wire(cls.O[i], reg.O)
                    if has_ce:
                        wire(cls.CE, reg.CE)
                    if has_reset:
                        wire(cls.RESET, reg.RESET)

    return _Register

def RegisterAnyType(t: Kind, init: int = 0, has_ce: bool = False, has_reset: bool = False):
    return DefineRegisterAnyType(t, init, has_ce, has_reset)()


