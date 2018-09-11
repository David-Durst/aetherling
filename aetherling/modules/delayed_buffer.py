from aetherling.helpers.nameCleanup import cleanName
from magma import *
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
    """

    class _RAM(Circuit):

        out_per_clock = max(n // total_emitting_period, 1)
        if n % k != 0:
            raise Exception("Total number of elements must be divisible by inputs per"
                            "active clock. Instead, n {} % k {} is {}".format(n, k, n % k))

        if total_emitting_period % n != 0 and n % total_emitting_period != 0:
            raise Exception("Total number of clocks must be divisible by total inputs."
                            "Instead, total_emitting_period {} % n {} is {}"
                            .format(total_emitting_period, n, total_emitting_period % n))

        if k % out_per_clock != 0:
            raise Exception("Emitted elements per clock must divide cleanly into input"
                            "elements per clock, but k {} % out_per_clock {} is {}"
                            .format(k, out_per_clock, k % out_per_clock))

        name = 'RAM_{}t_{}n_{}k_{}emittingPeriod'.format(cleanName(str(t)), n, k, total_emitting_period)
        IO = ['I', In(Array(k, t)), 'O', Out(Array(out_per_clock, t)), 'WE', In(Bit)] + ClockInterface(has_ce=True)
        @classmethod
        def definition(cls):
            # a counter that tracks the current clock in the total emitting period
            point_in_emitting_period = SizedCounterModM(total_emitting_period, has_ce=True)

            rams = MapParallel(cirb, k, RAMAnyType(cirb, t, n // k))

            # each clock WE is set, write to the RAMs and increment the address
            writing_location_per_bank = SizedCounterModM(n // k, has_ce=True)
            wire(cls.I, rams.WDATA)
            for i in range(k):
                wire(writing_location_per_bank.out, rams.WADDR[i])
                wire(cls.WE, rams.WE)
            wire(cls.WE, writing_location_per_bank.CE)

            # divide point in the current emitting period by n to get current element
            current_element = point_in_emitting_period.out / n
            # then get the current element to emit per each bank and then the right bank to emit
            current_element_per_banked_ram = current_element / k
            current_bank = point_in_emitting_period.out % k

            for i in range(k):
                wire(current_element_per_banked_ram, rams.RADDR[i])

            ram_bank_selector = MuxAnyType(cirb, Array(cls.out_per_clock, t), k)
            for i in range(k):
                wire(rams.RDATA[i], ram_bank_selector.data[i // cls.out_per_clock][i % cls.out_per_clock])
            wire(current_bank, ram_bank_selector.sel)

            # remove latency of RAMs of by emitting first input on first clock immediately
            first_input_or_rams = MuxAnyType(cirb, t, 2)
            wire(ram_bank_selector.out, first_input_or_rams.data)
            wire(point_in_emitting_period.out == 0, first_input_or_rams.sel)

            wire(first_input_or_rams.out, cls.O)

    return _RAM

def DelayedBuffer(cirb: CoreIRBackend, t: Kind, n: int, total_emitting_period: int):
    return DefineDelayedBuffer(cirb, t, n, total_emitting_period)()
