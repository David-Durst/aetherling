from aetherling.modules.counter import AESizedCounterModM
from aetherling.modules.initial_delay_counter import DefineInitialDelayCounter
from mantle.coreir import DefineCoreirConst
from magma import *
from magma.circuit import DefineCircuitKind
from aetherling.helpers.nameCleanup import cleanName
from aetherling.space_time.space_time_types import *
from aetherling.space_time.type_helpers import valid_ports
from aetherling.modules.ram_any_type import DefineRAMAnyType

@cache_definition
def DefineFIFO(t: ST_Type, delay: int,
               has_ce: bool = False, has_reset: bool = False, has_valid: bool = False) -> DefineCircuitKind:
    """
    A FIFO to delay a space-time type t by delay clocks

    I : In(T)
    O : Out(T)

    if has_ce:
    CE : In(Bit)
    if has_reset:
    RESET : In(Bit)
    if has_valid:
    valid_up : In(Bit)
    valid_down : Out(Bit)
    """
    class _FIFO(Circuit):
        name = "FIFO_t{}_delay{}_hasCE{}_hasReset{}_hasValid{}".format(cleanName(str(t)), str(delay),
                                                                          str(has_ce), str(has_reset),
                                                                          str(has_valid))
        IO = ['I', In(t.magma_repr()), 'O', Out(t.magma_repr())] + ClockInterface(has_ce, has_reset)
        if has_valid:
            IO += valid_ports
        @classmethod
        def definition(cls):
            enabled = DefineCoreirConst(1, 1)().O[0]
            if has_valid:
                enabled = cls.valid_up & enabled
            if has_ce:
                enabled = bit(cls.CE) & enabled

            per_clock_type = t.magma_repr()
            read_counter = AESizedCounterModM(delay, has_ce=True, has_reset=has_reset)
            write_counter = AESizedCounterModM(delay, has_ce=True, has_reset=has_reset)
            fifo_buffer = DefineRAMAnyType(per_clock_type, delay)()

            # delay read for delay clocks
            internal_delay_counter = DefineInitialDelayCounter(delay, has_ce=True, has_reset=has_reset)()
            advance_read_counter = internal_delay_counter.valid
            wire(enabled, internal_delay_counter.CE)
            wire(advance_read_counter & enabled, read_counter.CE)
            wire(enabled, write_counter.CE)

            if has_reset:
                wire(cls.RESET, read_counter.RESET)
                wire(cls.RESET, write_counter.RESET)
                wire(cls.RESET, internal_delay_counter.RESET)

            wire(fifo_buffer.WADDR, write_counter.O)
            wire(fifo_buffer.RADDR, read_counter.O)
            wire(fifo_buffer.WDATA, cls.I)
            wire(fifo_buffer.RDATA, cls.O)
            wire(fifo_buffer.WE, enabled)

            wire(advance_read_counter, cls.valid_down)

    return _FIFO

def FIFO(t: ST_Type, delay: int,
         has_ce: bool = False, has_reset: bool = False, has_valid: bool = False) -> DefineCircuitKind:
    return DefineFIFO(t, delay, has_ce, has_reset, has_valid)()

