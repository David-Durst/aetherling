from aetherling.space_time.space_time_types import *
from aetherling.space_time.nested_counters import *
from aetherling.modules.ram_any_type import *
from aetherling.modules.term_any_type import TermAnyType
from aetherling.modules.mux_any_type import DefineMuxAnyType
from aetherling.modules.map_fully_parallel_sequential import DefineNativeMapParallel
from aetherling.helpers.nameCleanup import cleanName
from mantle.coreir.memory import getRAMAddrWidth
from mantle.common.countermod import Decode
from aetherling.modules.ram_any_type import *
from magma import *
from magma.circuit import DefineCircuitKind, Circuit

__all__ = ['DefineRAM_ST', 'RAM_ST']

@cache_definition
def DefineRAM_ST(t: ST_Type, n: int, has_reset = False, read_latency = 0) -> DefineCircuitKind:
    """
    Generate a RAM where t store n objects each of type t.
    WE, RE and RESET affect where in a t is being written/read.
    This is different from normal magma RAMs that don't have values that take multiple clocks.


    RADDR : In(Array[log_2(n), Bit)],
    RDATA : Out(t.magma_repr()),
    WADDR : In(Array(log_2(n), Bit)),
    WDATA : In(t.magma_repr()),
    WE: In(Bit)
    RE: In(Bit)

    if has_reset:
    RESET : In(Bit)
    """

    class _RAM_ST(Circuit):
        name = 'RAM_ST_{}_hasReset{}'.format(cleanName(str(t)), str(has_reset))
        addr_width = getRAMAddrWidth(n)
        IO = ['RADDR', In(Bits[addr_width]),
              'RDATA', Out(t.magma_repr()),
              'WADDR', In(Bits[addr_width]),
              'WDATA', In(t.magma_repr()),
              'WE', In(Bit),
              'RE', In(Bit)
              ] + ClockInterface(has_ce=False, has_reset=has_reset)
        @classmethod
        def definition(cls):
            # each valid clock, going to get a magma_repr in
            # read or write each one of those to a location
            rams = [DefineRAMAnyType(t.magma_repr(), t.valid_clocks(), read_latency=read_latency)() for _ in range(n)]
            read_time_position_counter = DefineNestedCounters(t, has_cur_valid=True, has_ce=True, has_reset=has_reset)()
            read_valid_term = TermAnyType(Bit)
            read_last_term = TermAnyType(Bit)
            write_time_position_counter = DefineNestedCounters(t, has_cur_valid=True, has_ce=True, has_reset=has_reset)()
            write_valid_term = TermAnyType(Bit)
            write_last_term = TermAnyType(Bit)
            read_selector = DefineMuxAnyType(t.magma_repr(), n)()

            for i in range(n):
                wire(cls.WDATA, rams[i].WDATA)
                wire(write_time_position_counter.cur_valid, rams[i].WADDR)
                wire(read_selector.data[i], rams[i].RDATA)
                wire(read_time_position_counter.cur_valid, rams[i].RADDR)
                write_cur_ram = Decode(i, cls.WADDR.N)(cls.WADDR)
                wire(write_cur_ram & write_time_position_counter.valid, rams[i].WE)

            wire(cls.RADDR, read_selector.sel)
            wire(cls.RDATA, read_selector.out)

            wire(cls.WE, write_time_position_counter.CE)
            wire(cls.RE, read_time_position_counter.CE)

            wire(read_time_position_counter.valid, read_valid_term.I)
            wire(read_time_position_counter.last, read_last_term.I)
            wire(write_time_position_counter.valid, write_valid_term.I)
            wire(write_time_position_counter.last, write_last_term.I)

            if has_reset:
                wire(cls.RESET, write_time_position_counter.RESET)
                wire(cls.RESET, read_time_position_counter.RESET)


    return _RAM_ST

def RAM_ST(t: ST_Type, n: int, has_reset: bool = False) -> Circuit:
    DefineRAM_ST(t, n, has_reset)
