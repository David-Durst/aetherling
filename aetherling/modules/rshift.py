from mantle import Register, Decode
from mantle.common.countermod import SizedCounterModM
from mantle.common.operator import *
from aetherling.modules.term_any_type import TermAnyType
from aetherling.modules.fifo import FIFO
from magma import *
from magma.circuit import DefineCircuitKind
from .hydrate import Dehydrate, Hydrate
from .map_fully_parallel_sequential import MapParallel
from ..helpers.nameCleanup import cleanName
from ..helpers.magma_helpers import ready_valid_interface

__all__ = ['DefineRShiftParallel', 'RShiftParallel',
           'DefineRShiftSequential', 'RShiftSequential']

@cache_definition
def DefineRShiftParallel(n: int, init: DefineCircuitKind, shift_amount: int, T: Kind, has_ready_valid=False):
    """
    Shifts the elements in SSeq n T' by shift_amount to the right.
    init is a Magma circuit that specifies the outputs used for the shift_amount elements to the right

    The time_per_element clock cycles in a period is not relevant for this operator as it is combinational.

    Note that the T passed to this operator just the Magma type each clock cycle.
    You can get T by calling magma_repr on a space-time type T'.

    I : In(Array[n, T])
    O : Out(Array[n, T])
    if has_ready_valid:
    ready_up : Out(Bit)
    valid_up : In(Bit)
    ready_down : In(Bit)
    valid_down : Out(Bit)
    """
    class RShiftParallel(Circuit):
        name = "RShiftParallel_n{}_init{}_shift_amount{}_T_rv{}".format(str(n), init.name,
                                                                        str(shift_amount), cleanName(str(T)),
                                                                        str(has_ready_valid))
        IO = ['I', In(Array[n, T]), 'O', Out(Array[n, T])]
        if has_ready_valid:
            IO += ready_valid_interface
        @classmethod
        def definition(rshiftParallel):
            # dehydrate all but the first, that one is passed through
            inputs_term = TermAnyType(Array[shift_amount, T])
            for i in range(n):
                if i >= shift_amount:
                    wire(rshiftParallel.I[i], rshiftParallel.O[i + shift_amount])
                else:
                    wire(rshiftParallel.I[i], inputs_term.I[i - shift_amount])
            if has_ready_valid:
                wire(rshiftParallel.ready_up, rshiftParallel.ready_down)
                wire(rshiftParallel.valid_up, rshiftParallel.valid_down)
    return RShiftParallel

def RShiftParallel(n: int, init: DefineCircuitKind, shift_amount: int, T: Kind, has_ready_valid=False):
    return DefineRShiftParallel(n, init, shift_amount, T, has_ready_valid)()

@cache_definition
def DefineRShiftSequential(n, time_per_element, idx, T, has_ce=False, has_reset=False):
    """
    Shifts the elements in TSeq n T' by shift_amount to the right.
    init is a Magma circuit that specifies the outputs used for the shift_amount elements to the right

    The time_per_element clock cycles in a period is not relevant for this operator as it is combinational.

    Note that the T passed to this operator just the Magma type each clock cycle.
    You can get T by calling magma_repr on a space-time type T'.

    I : In(Array[n, T])
    O : Out(Array[n, T])
    if has_ready_valid:
    ready_up : Out(Bit)
    valid_up : In(Bit)
    ready_down : In(Bit)
    valid_down : Out(Bit)
    """
    class DownsampleSequential(Circuit):
        name = "DownsampleSequential_n{}_tEl{}_idx{}_T{}_hasCE{}_hasReset{}".format(str(n), str(time_per_element), \
                                                                                    str(idx), cleanName(str(T)), str(has_ce), str(has_reset))
        IO = ['I', In(T), 'O', Out(T)] + ClockInterface(has_ce, has_reset) + ready_valid_interface
        @classmethod
        def definition(rshiftSequential):
            # the counter of the current element of output sequence, when hits 0, load the next input to upsample
            element_idx_counter = SizedCounterModM(n, has_ce=True, has_reset=has_reset)
            is_first_element = Decode(0, element_idx_counter.O.N)(element_idx_counter.O)
            if has_reset:
                wire(rshiftSequential.RESET, element_idx_counter.RESET)

            # ready means can accept input when get valid from upstream
            # do this when in first element or downstream ready to accept
            ready = is_first_element & rshiftSequential.ready_down
            # valid means can emit downstream
            # valid when in first element and upstream valid or repeating old data
            valid = (is_first_element & rshiftSequential.valid_up) | (~is_first_element)

            # only assert these signals when CE is high or no CE
            if has_ce:
                enabled = enabled & bit(rshiftSequential.CE)
                ready = ready & bit(rshiftSequential.CE)
                valid = valid & bit(rshiftSequential.CE)

            if n * time_per_element > 1:
                value_store = FIFO(n, time_per_element, T, True, has_ce, has_reset)()
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

                if has_ce:
                    wire(value_store.ce, rshiftSequential.CE)

                if has_reset:
                    wire(time_per_element_counter.RESET, rshiftSequential.RESET)

            else:
                value_store = DefineRegisterAnyType(T, has_ce=True)()
                value_store_input = value_store.I
                value_store_output = value_store.O

                wire(element_idx_counter.CE, enabled)
                wire(value_store.CE, is_first_element & enabled)

            output_selector = DefineMuxAnyType(T, 2)()

            wire(rshiftSequential.I, value_store_input)

            # on first element, send the input directly out. otherwise, use the register
            wire(is_first_element, output_selector.sel[0])
            wire(value_store_output, output_selector.data[0])
            wire(rshiftSequential.I, output_selector.data[1])
            wire(output_selector.out, rshiftSequential.O)

            wire(valid, rshiftSequential.valid_down)
            wire(ready, rshiftSequential.ready_up)
    return DownsampleSequential

def RShiftSequential(n, time_per_element, idx, T, has_ce=False, has_reset=False):
    return DefineRShiftSequential(n, time_per_element, idx, T, has_ce, has_reset)()
