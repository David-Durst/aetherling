from aetherling.helpers.nameCleanup import cleanName
from magma import *
from mantle import DefineCoreirConst
from magma.backend.coreir_ import CoreIRBackend
from aetherling.modules.map_fully_parallel_sequential import MapParallel
from aetherling.modules.ram_any_type import RAMAnyType
from aetherling.modules.mux_any_type import MuxAnyType
from mantle.common.countermod import SizedCounterModM


__all__ = ['DefineDelayedBuffer', 'DelayedBuffer']

@cache_definition
def DefineDelayedBuffer(cirb: CoreIRBackend, t: Kind, n: int, k: int, total_emitting_period: int):
    """
    Generate a buffer that accepts n items, k at a time, emtting them evenly spaced over a total_emitting_period
     number of clocks.

    I: In(Array(k, t)), O: Out(t), WE: In(Bit), CE: In(Enable)

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

        name = 'DelayedBuffer_{}t_{}n_{}k_{}emittingPeriod'.format(cleanName(str(t)), n, k, total_emitting_period)
        IO = ['I', In(Array(k, t)), 'O', Out(Array(out_per_clock, t)), 'WE', In(Bit),
              'valid', Out(Bit)] + ClockInterface(has_ce=True)
        @classmethod
        def definition(cls):
            rams = MapParallel(cirb, k, RAMAnyType(cirb, t, n // k))

            # each clock WE is set, write to the RAMs and increment the address
            writing_location_per_bank = SizedCounterModM(n // k, has_ce=True)
            wire(cls.I, rams.WDATA)
            for i in range(k):
                wire(writing_location_per_bank.O, rams.WADDR[i])
                wire(cls.WE, rams.WE[i])
            wire(cls.WE, writing_location_per_bank.CE)

            # the bank ram counter tracks which entry in all the banked rams RADDR should be set to
            ticks_per_element = total_emitting_period // n
            ticks_per_row_of_elements = ticks_per_element // k
            # this completes a cycle ever time the current_element_per_bank_ram increments by 1
            if n // k == 1:
                current_element_per_banked_ram_counter = DefineCoreirConst(1, 0)()
            elif ticks_per_row_of_elements == 1:
                current_element_per_banked_ram_counter = SizedCounterModM(n // k, has_ce=True)
                wire(cls.CE, current_element_per_banked_ram_counter.CE)
            else:
                bank_ram_tick_counter = SizedCounterModM(ticks_per_row_of_elements, has_ce=True)
                ticks_per_row_of_elements_const = DefineCoreirConst(len(bank_ram_tick_counter.O), ticks_per_row_of_elements - 1)()

                current_element_per_banked_ram_counter = SizedCounterModM(n // k, has_ce=True)
                wire(cls.CE, bank_ram_tick_counter.CE)
                wire(bit(cls.CE) & (bank_ram_tick_counter.O == ticks_per_row_of_elements_const.O),
                     current_element_per_banked_ram_counter.CE)

            for i in range(k):
                wire(current_element_per_banked_ram_counter.O, rams.RADDR[i])

            # the mux bank selector counter tracks which of the banks to read from right now

            # divide the number of ticks per row by the number of mux outputs per row
            # (k // cls.out_per_clock) to get ticks per mux output
            outputs_per_row = k // cls.out_per_clock
            ticks_per_mux_output = ticks_per_row_of_elements // outputs_per_row
            if ticks_per_mux_output == 1:
                ticks_per_mux_counter = DefineCoreirConst(1, 0)()
            else:
                ticks_per_mux_counter = SizedCounterModM(ticks_per_mux_output, has_ce=True)

            # this counter completes a cycle once for every mux output
            if outputs_per_row == 1:
                mux_bank_selector_counter = DefineCoreirConst(1, 0)()
            elif ticks_per_mux_output == 1:
                mux_bank_selector_counter = SizedCounterModM(outputs_per_row, has_ce=True)
                wire(cls.CE, mux_bank_selector_counter.CE)
            else:
                ticks_per_mux_output_const = DefineCoreirConst(len(ticks_per_mux_counter.O), ticks_per_mux_output - 1)()

                mux_bank_selector_counter = SizedCounterModM(outputs_per_row, has_ce=True)
                wire(cls.CE, ticks_per_mux_counter.CE)
                wire(bit(cls.CE) & (ticks_per_mux_counter.O == ticks_per_mux_output_const.O),
                     mux_bank_selector_counter.CE)

            ram_bank_selector = MuxAnyType(cirb, Array(cls.out_per_clock, t), k // cls.out_per_clock)
            for i in range(k):
                wire(rams.RDATA[i], ram_bank_selector.data[i // cls.out_per_clock][i % cls.out_per_clock])
            wire(mux_bank_selector_counter.O, ram_bank_selector.sel)

            # remove latency of RAMs of by emitting first input on first clock immediately
            first_input_or_rams = MuxAnyType(cirb, Array(cls.out_per_clock, t), 2)
            wire(cls.I[0:cls.out_per_clock], first_input_or_rams.data[1])
            wire(ram_bank_selector.out, first_input_or_rams.data[0])

            # emit input directly only on first clock
            # a counter that tracks the current clock in the total emitting period
            point_in_emitting_period = SizedCounterModM(total_emitting_period, has_ce=True)
            zero_const = DefineCoreirConst(len(point_in_emitting_period.O), 0)()

            wire(point_in_emitting_period.O == zero_const.O, first_input_or_rams.sel[0])

            wire(first_input_or_rams.out, cls.O)

            # valid on first enabled clock where on new output of mux
            zero_const = DefineCoreirConst(len(ticks_per_mux_counter.O), 0)()

            wire(bit(cls.CE) & (ticks_per_mux_counter.O == zero_const.O), cls.valid)

    return _DelayedBuffer

def DelayedBuffer(cirb: CoreIRBackend, t: Kind, n: int, k: int, total_emitting_period: int):
    return DefineDelayedBuffer(cirb, t, n, k, total_emitting_period)()
