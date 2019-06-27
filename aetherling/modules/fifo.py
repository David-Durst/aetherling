from mantle import Decode
from mantle.common.countermod import SizedCounterModM
from magma import *
from ..helpers.nameCleanup import cleanName
from ..helpers.magma_helpers import ready_valid_interface
from aetherling.modules.register_any_type import DefineRegisterAnyType
from aetherling.modules.mux_any_type import DefineMuxAnyType
from aetherling.modules.term_any_type import DefineTermAnyType
from aetherling.modules.ram_any_type import DefineRAMAnyType

@cache_definition
def DefineFIFO(n, time_per_element, T, has_ce=False, has_reset=False):
    """
    A FIFO to store n elements of type T

    You can get time_per_element by calling time on a space-time type.

    Note that the T passed to this operator just the Magma type each clock cycle.
    You can get T by calling magma_repr on a space-time type T'.

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
        IO = ['I', In(T), 'O', Out(T)] + ClockInterface(has_ce, has_reset) + ready_valid_interface
        @classmethod
        def definition(fifo):
            read_counter = SizedCounterModM(n * time_per_element, has_ce=True, has_reset=has_reset)
            write_counter = SizedCounterModM(n * time_per_element, has_ce=True, has_reset=has_reset)
            if has_reset:
                wire(fifo.RESET, read_counter.RESET)
                wire(fifo.RESET, write_counter.RESET)

            # ready means can accept input.
            # Do this when the read_counter+1 when get valid from upstream
            # do this when in first element and downstream ready to accept
            ready = is_first_element & upsampleSequential.ready_down
            # valid means can emit downstream
            # valid when in first element and upstream valid or repeating old data
            valid = (is_first_element & upsampleSequential.valid_up) | (~is_first_element)

            # only assert these signals when CE is high or no CE
            if has_ce:
                enabled = enabled & bit(upsampleSequential.CE)
                ready = ready & bit(upsampleSequential.CE)
                valid = valid & bit(upsampleSequential.CE)

            if time_per_element > 1:
                value_store = DefineRAMAnyType(T, time_per_element)()
                value_store_input = value_store.WDATA
                value_store_output = value_store.RDATA

                time_per_element_counter = SizedCounterModM(time_per_element,
                                                            has_ce=True, has_reset=has_reset)
                go_to_next_element = Decode(time_per_element - 1, time_per_element_counter.O.N)(time_per_element_counter.O)

                wire(time_per_element_counter.CE, enabled)
                wire(element_idx_counter.CE, enabled & go_to_next_element)
                wire(value_store.WE, is_first_element & enabled)
                # location in current element is where to read and write.
                # will write on first iteration through element, read on later iterations
                wire(time_per_element_counter.O, value_store.WADDR)
                wire(time_per_element_counter.O, value_store.RADDR)

                if has_reset:
                    wire(time_per_element_counter.RESET, upsampleSequential.RESET)

            else:
                value_store = DefineRegisterAnyType(T, has_ce=True)()
                value_store_input = value_store.I
                value_store_output = value_store.O

                wire(element_idx_counter.CE, enabled)
                wire(value_store.CE, is_first_element & enabled)

            output_selector = DefineMuxAnyType(T, 2)()

            wire(upsampleSequential.I, value_store_input)

            # on first element, send the input directly out. otherwise, use the register
            wire(is_first_element, output_selector.sel[0])
            wire(value_store_output, output_selector.data[0])
            wire(upsampleSequential.I, output_selector.data[1])
            wire(output_selector.out, upsampleSequential.O)

            wire(valid, upsampleSequential.valid_down)
            wire(ready, upsampleSequential.ready_up)

    return FIFO

def FIFO(n, time_per_element, T, has_ce=False, has_reset=False):
    return DefineFIFO(n, time_per_element, T, has_ce, has_reset)()

