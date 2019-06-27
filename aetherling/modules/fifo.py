from mantle import Decode
from mantle.common.countermod import SizedCounterModM
from mantle.common.counter import CeilFloorUpDownCounter
from magma import *
from ..helpers.nameCleanup import cleanName
from ..helpers.magma_helpers import ready_valid_interface
from aetherling.modules.ram_any_type import DefineRAMAnyType

@cache_definition
def DefineFIFO(n, time_per_element, T, expand_max_by_one_clk, has_ce=False, has_reset=False):
    """
    A FIFO to store n elements of type T

    You can get time_per_element by calling time on a space-time type.

    Note that the T passed to this operator just the Magma type each clock cycle.
    You can get T by calling magma_repr on a space-time type T'.

    Note: Normally, if the FIFO is fully, you cannot write to it, even if reading from it.
    This is to break ready-valid chains. If you could write and read while at full capacity, the ready
    signal passed upstream, which indicates when a write can happen, woudl need to account for the downstream ready,
    which indicates when a read is happening. The direct propagation of ready-valid signals means the combinational
    chains of ready-valid signals aren't broken.

    To allow reading and writing on the same cycle at the max number of elements, turn on the
    expland_max_by_one_clk flag. This will make the buffer 1 cycle larger than the max size.
    This is used as a flag here as this increases the size by 1

    I : In(T)
    O : Out(T)
    ready_up : Out(Bit)
    valid_up : In(Bit)
    ready_down : In(Bit)
    valid_down : Out(Bit)
    if has_ce:
    CE : In(Bit)
    if has_reset:
    RESET : In(Bit)
    """
    class FIFO(Circuit):
        name = "FIFO_n{}_tEl{}_T{}_hasCE{}_hasReset{}".format(str(n), str(time_per_element), \
                                                              cleanName(str(T)), str(has_ce), str(has_reset))
        IO = ['I', In(T), 'O', Out(T), 'rc', Out(Array[3, Bit]), 'wc', Out(Array[3, Bit]), 'nsc', Out(Array[3, Bit])] + ClockInterface(has_ce, has_reset) + ready_valid_interface
        @classmethod
        def definition(fifo):
            pieces_of_elements = n * time_per_element
            if expand_max_by_one_clk:
                pieces_of_elements += 1
            read_counter = SizedCounterModM(pieces_of_elements, has_ce=True, has_reset=has_reset)
            write_counter = SizedCounterModM(pieces_of_elements, has_ce=True, has_reset=has_reset)
            # add 1 as 0 doesn't mean on first element, means 0 element, so also need pieces_of_elements to be
            # entry if all written
            num_stored_counter = CeilFloorUpDownCounter(pieces_of_elements + 1, has_ce=has_ce, has_reset=has_reset)
            wire(read_counter, fifo.rc)
            wire(write_counter, fifo.wc)
            wire(num_stored_counter, fifo.nsc)

            # ready means can accept input.
            # Do this when the num_stored_counter is not at it's max value
            ready = ~Decode(pieces_of_elements, num_stored_counter.O.N)(num_stored_counter.O)
            # valid means can emit downstream
            # Do this when the num_stored_counter shows not empty
            valid = ~Decode(0, num_stored_counter.O.N)(num_stored_counter.O)

            # only assert these signals when CE is high or no CE
            if has_ce:
                ready = ready & bit(fifo.CE)
                valid = valid & bit(fifo.CE)
                wire(num_stored_counter.CE, fifo.CE)

            read_this_clk = valid & fifo.ready_down
            write_this_clk = ready & fifo.valid_up
            wire(read_counter.CE, read_this_clk)
            wire(write_counter.CE, write_this_clk)
            wire(num_stored_counter.U, write_this_clk)
            wire(num_stored_counter.D, read_this_clk)

            if has_reset:
                wire(fifo.RESET, read_counter.RESET)
                wire(fifo.RESET, write_counter.RESET)
                wire(fifo.RESET, num_stored_counter.RESET)

            value_store = DefineRAMAnyType(T, pieces_of_elements)()
            wire(value_store.WADDR, write_counter.O)
            wire(value_store.RADDR, read_counter.O)
            wire(value_store.WDATA, fifo.I)
            wire(value_store.RDATA, fifo.O)
            wire(value_store.WE, write_this_clk)

            wire(valid, fifo.valid_down)
            wire(ready, fifo.ready_up)

    return FIFO

def FIFO(n, time_per_element, T, expand_max_by_one_clk, has_ce=False, has_reset=False):
    return DefineFIFO(n, time_per_element, T, expand_max_by_one_clk, has_ce, has_reset)()

