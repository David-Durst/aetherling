from aetherling.helpers.nameCleanup import cleanName
from magma import *
from mantle import DefineCoreirConst
from aetherling.modules.map_fully_parallel_sequential import MapParallel, DefineMapParallel, DefineNativeMapParallel
from aetherling.modules.ram_any_type import DefineRAMAnyType
from aetherling.modules.mux_any_type import MuxAnyType
from aetherling.modules.initial_delay_counter import InitialDelayCounter
from mantle.common.countermod import SizedCounterModM
import math


__all__ = ['DefineDelayedBuffer', 'DelayedBuffer', 'GetDBDebugInterface']

def GetDBDebugInterface(t, n, k):
    test_ram = DefineMapParallel(k, DefineRAMAnyType(t, n // k))
    debug_interface = ['WDATA', Out(type(test_ram.WDATA)), 'RDATA', Out(type(test_ram.WDATA)),
                       'WADDR', Out(type(test_ram.WADDR)), 'RADDR', Out(type(test_ram.WADDR)),
                       'RAMWE', Out(Bit)]
    return debug_interface

@cache_definition
def DefineDelayedBuffer(t: Kind, n: int, k: int, total_emitting_period: int,
                        initial_emitting_delay: int = 0, add_debug_interface = False):
    """
    Generate a buffer that accepts n items, k at a time, emtting them evenly spaced over a total_emitting_period
     number of clocks. Initial_emitting_delay states to wait that many clocks the first period between getting
     the first input and emitting the first output. This ensures a delay between inputs and outputs.

     Note: This buffer does not verify the user won't overwrite its contents before they are read if
     initial_emitting_delay > 0. One could fill up the buffer by having total_emitting_period 4, n = 2, k = 2,
     initial_emitting_delay 6. Input would be provided on clock 0 and 4. Before the first input is emitted on
     clock 5, it would be overwitten with the second input.

    I: In(Array[k, t]), O: Out(t), WE: In(Bit), CE: In(Enable)

    Restrictions:

    1. n % k == 0
    2. total_emitting_period % n == 0
    3. k % out_per_clock == 0 (out_per_clock = max(n // total_emitting_period, 1)
    """

    class _DelayedBuffer(Circuit):

        out_per_clock = max(n // total_emitting_period, 1)
        if n % k != 0:
            raise Exception("Total number of elements must be divisible by inputs per"
                            "active clock. Instead, n {} % k {} is {}".format(n, k, n % k))

        if total_emitting_period % n != 0 and n % total_emitting_period != 0:
            raise Exception("Total number of clocks must be divisible by total inputs. "
                            "Instead, total_emitting_period {} % n {} is {}"
                            .format(total_emitting_period, n, total_emitting_period % n))

        if k % out_per_clock != 0:
            raise Exception("Emitted elements per clock must divide cleanly into input"
                            "elements per clock, but k {} % out_per_clock {} is {}. "
                            "Note: out_per_clock = max(n // total_emitting_period, 1)"
                            .format(k, out_per_clock, k % out_per_clock))

        name = 'DelayedBuffer_{}t_{}n_{}k_{}emittingPeriod_{}initialDelay'.format(cleanName(str(t)), n, k,
                                                                                 total_emitting_period, initial_emitting_delay)
        if add_debug_interface:
            debug_interface = GetDBDebugInterface(t, n, k)
        else:
            debug_interface = []
        IO = ['I', In(Array[k, t]), 'O', Out(Array[out_per_clock, t]), 'WE', In(Bit),
              'valid', Out(Bit)] + debug_interface + ClockInterface(has_ce=True)
        @classmethod
        def definition(cls):
            rams = DefineNativeMapParallel(k, DefineRAMAnyType(t, n // k))()

            # each clock WE is set, write to the RAMs and increment the address
            writing_location_per_bank = SizedCounterModM(n // k, has_ce=True)
            wire(cls.I, rams.WDATA)
            ramEnableWire = cls.WE & bit(cls.CE)
            if add_debug_interface:
                wire(cls.I, cls.WDATA)
                wire(ramEnableWire, cls.RAMWE)
            for i in range(k):
                wire(writing_location_per_bank.O, rams.WADDR[i])
                if add_debug_interface:
                    wire(writing_location_per_bank.O, cls.WADDR[i])
                wire(ramEnableWire, rams.WE[i])
            wire(cls.WE & bit(cls.CE), writing_location_per_bank.CE)

            if initial_emitting_delay > 0:
                initial_delay_counter = InitialDelayCounter(initial_emitting_delay)
                ce_with_delay = bit(cls.CE) & initial_delay_counter.valid
            else:
                ce_with_delay = bit(cls.CE)

            # the bank ram counter tracks which group of entries in all the banked rams RADDR should be set to
            ticks_per_element = total_emitting_period // (n // cls.out_per_clock)
            ticks_per_row_of_elements = ticks_per_element * (k // cls.out_per_clock)
            # this completes a cycle ever time the current_element_per_bank_ram increments by 1
            if n // k == 1:
                current_element_per_banked_ram_counter = DefineCoreirConst(1, 0)()
            elif ticks_per_row_of_elements == 1:
                current_element_per_banked_ram_counter = SizedCounterModM(n // k, has_ce=True)
                wire(ce_with_delay, current_element_per_banked_ram_counter.CE)
            else:
                bank_ram_tick_counter = SizedCounterModM(ticks_per_row_of_elements, has_ce=True)
                ticks_per_row_of_elements_const = DefineCoreirConst(len(bank_ram_tick_counter.O), ticks_per_row_of_elements - 1)()

                current_element_per_banked_ram_counter = SizedCounterModM(n // k, has_ce=True)
                wire(ce_with_delay, bank_ram_tick_counter.CE)
                wire(ce_with_delay & (bank_ram_tick_counter.O == ticks_per_row_of_elements_const.O),
                     current_element_per_banked_ram_counter.CE)

            for i in range(k):
                wire(current_element_per_banked_ram_counter.O, rams.RADDR[i])
                if add_debug_interface:
                    wire(current_element_per_banked_ram_counter.O, cls.RADDR[i])

            # the mux bank selector counter tracks which of the banks to read from right now

            # divide the number of ticks per row by the number of mux outputs per row
            # (k // cls.out_per_clock) to get ticks per mux output
            outputs_per_row = k // cls.out_per_clock
            ticks_per_mux_output = ticks_per_row_of_elements // outputs_per_row
            if ticks_per_mux_output == 1:
                ticks_per_mux_counter = DefineCoreirConst(1, 0)()
            else:
                ticks_per_mux_counter = SizedCounterModM(ticks_per_mux_output, has_ce=True)
                wire(ce_with_delay, ticks_per_mux_counter.CE)

            # this counter completes a cycle once for every mux output
            if outputs_per_row == 1:
                mux_bank_selector_counter = DefineCoreirConst(1, 0)()
            elif ticks_per_mux_output == 1:
                mux_bank_selector_counter = SizedCounterModM(outputs_per_row, has_ce=True)
                wire(ce_with_delay, mux_bank_selector_counter.CE)
            else:
                ticks_per_mux_output_const = DefineCoreirConst(len(ticks_per_mux_counter.O), ticks_per_mux_output - 1)()

                mux_bank_selector_counter = SizedCounterModM(outputs_per_row, has_ce=True)
                wire(ce_with_delay & (ticks_per_mux_counter.O == ticks_per_mux_output_const.O),
                     mux_bank_selector_counter.CE)

            ram_bank_selector = MuxAnyType(Array[cls.out_per_clock, t], k // cls.out_per_clock)
            for i in range(k):
                wire(rams.RDATA[i], ram_bank_selector.data[i // cls.out_per_clock][i % cls.out_per_clock])
                if add_debug_interface:
                    wire(rams.RDATA[i], cls.RDATA[i])
            wire(mux_bank_selector_counter.O, ram_bank_selector.sel)

            # if not delaying,
            # remove latency of RAMs of by emitting first input on first clock immediately
            if initial_emitting_delay == 0:
                first_input_or_rams = MuxAnyType(Array[cls.out_per_clock, t], 2)
                wire(cls.I[0:cls.out_per_clock], first_input_or_rams.data[1])
                wire(ram_bank_selector.out, first_input_or_rams.data[0])

                # emit input directly only on first clock
                # a counter that tracks the current clock in the total emitting period
                point_in_emitting_period = SizedCounterModM(total_emitting_period, has_ce=True)
                zero_const = DefineCoreirConst(len(point_in_emitting_period.O), 0)()

                # don't delay this if delaying output, as then don't want input, want whatever ram says.
                wire(cls.CE, point_in_emitting_period.CE)

                wire(point_in_emitting_period.O == zero_const.O, first_input_or_rams.sel[0])

                wire(first_input_or_rams.out, cls.O)
            else:
                wire(ram_bank_selector.out, cls.O)

            # valid on first enabled clock where on new output of mux
            zero_const = DefineCoreirConst(len(ticks_per_mux_counter.O), 0)()

            wire(bit(ce_with_delay) & (ticks_per_mux_counter.O == zero_const.O), cls.valid)

    return _DelayedBuffer

def DelayedBuffer(t: Kind, n: int, k: int, total_emitting_period: int,
                  initial_emitting_delay: int = 0, add_debug_interface = False):
    return DefineDelayedBuffer(t, n, k, total_emitting_period, initial_emitting_delay, add_debug_interface)()
