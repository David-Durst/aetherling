from aetherling.helpers.nameCleanup import cleanName
from magma import *
from magma.backend.coreir_ import CoreIRBackend
from mantle import DefineCoreirConst
from mantle.common.countermod import SizedCounterModM

__all__ = ['DefineInitialDelayCounter', 'InitialDelayCounter']

@cache_definition
def DefineInitialDelayCounter(num_clocks_delay: int):
    """
    Counter that emits invalid for num_delay_clocks, then always valid
    CE : In(Bit),  valid : Out(Bit)
    """

    class _Counter(Circuit):
        name = 'InitialDelayCounter_{}'.format(num_clocks_delay)
        IO = ['valid', Out(Bit)] + ClockInterface(has_ce=True)
        @classmethod
        def definition(cls):
            valid_counter = SizedCounterModM(max(num_clocks_delay, 2), has_ce=True)
            delay_const = DefineCoreirConst(len(valid_counter.O), num_clocks_delay)()
            wire(enable(bit(cls.CE) & (valid_counter.O < delay_const.O)), valid_counter.CE)
            wire(valid_counter.O == delay_const.O, cls.valid)

    return _Counter

def InitialDelayCounter(num_clocks_delay: int):
    return DefineInitialDelayCounter(num_clocks_delay)()
