from aetherling.space_time.space_time_types import *
from aetherling.space_time.nested_counters import *
from aetherling.modules.ram_any_type import *
from aetherling.helpers.nameCleanup import cleanName
from mantle.coreir.memory import getRAMAddrWidth
from mantle.common.countermod import Decode
from aetherling.modules.ram_any_type import *
from magma import *
from magma.circuit import DefineCircuitKind, Circuit

__all__ = ['DefineRAM_ST', 'RAM_ST']

@cache_definition
def DefineRAM_ST(t: ST_Type, n: int, has_reset = False) -> DefineCircuitKind:
    """
    Generate a RAM where t store n objects each of type t.
    WE, RE and RESET affect where in a t is being written/read.
    This is different from normal magma RAMs that don't have values that take multiple clocks.


    RADDR : In(Array[log_2(n), Bit)],
    RDATA : Out(t),
    WADDR : In(Array(log_2(n), Bit)),
    WDATA : In(t),
    WE: In(Bit)
    RE: In(Bit)

    if has_reset:
    RESET : In(Bit)
    """

    class _RAM_ST(Circuit):
        name = 'RAM_ST_{}_hasReset{}'.format(cleanName(str(t)), str(has_reset))
        addr_width = getRAMAddrWidth(n)
        IO = ['RADDR', In(Bits[addr_width]),
              'RDATA', Out(t),
              'WADDR', In(Bits[addr_width]),
              'WDATA', In(t),
              'WE', In(Bit),
              'RE', In(Bit)
              ] + ClockInterface(has_ce=False, has_reset=has_reset)
        @classmethod
        def definition(cls):
            # each valid clock, going to get a magma_repr in
            # read or write each one of those to a location
            no_time_ram = DefineRAMAnyType(Array[t.valid_clocks(), t.magma_repr()], n)()
            read_time_position_counter = DefineNestedCounters(t, has_cur_valid=True, has_ce=True, has_reset=has_reset)()
            write_time_position_counter = DefineNestedCounters(t, has_cur_valid=True, has_ce=True, has_reset=has_reset)()

            wire(cls.WDATA, no_time_ram.WDATA)
            wire(cls.WADDR + write_time_position_counter.cur_valid, no_time_ram.WADDR)
            wire(cls.RDATA, no_time_ram.RDATA)
            wire(cls.RADDR + read_time_position_counter.cur_valid, no_time_ram.RADDR)

            wire(cls.WE, write_time_position_counter.CE)
            wire(cls.WE, no_time_ram.WE)
            wire(cls.RE, read_time_position_counter.CE)

            if has_reset:
                wire(cls.RESET, write_time_position_counter.RESET)
                wire(cls.RESET, read_time_position_counter.RESET)


    return _RAM_ST

def RAM_ST(t: ST_Type, n: int, has_reset: bool = False) -> Circuit:
    DefineRAM_ST(t, n, has_reset)
