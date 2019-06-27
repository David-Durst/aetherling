from mantle import Register, Decode
from mantle.common.countermod import SizedCounterModM
from mantle.common.operator import *
from aetherling.modules.term_any_type import TermAnyType
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
    Shifts the elements in SSeq n T' by shift_amount to the right combinationally.
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
    Downsample a TSeq n T' to a TSeq 1 T' over n period.

    Each T' period is time_per_element clock cycles
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
    class DownsampleSequential(Circuit):
        name = "DownsampleSequential_n{}_tEl{}_idx{}_T{}_hasCE{}_hasReset{}".format(str(n), str(time_per_element), \
                                                                                    str(idx), cleanName(str(T)), str(has_ce), str(has_reset))
        IO = ['I', In(T), 'O', Out(T)] + ClockInterface(has_ce, has_reset) + ready_valid_interface
        @classmethod
        def definition(downsampleSequential):
            element_idx_counter = SizedCounterModM(n, has_ce=True, has_reset=has_reset)
            emit_cur_element = Decode(idx, element_idx_counter.O.N)(element_idx_counter.O)

            # enabled means run the circuit
            # do this when upstream is ready, so have something to downsample,
            # and when have to emit current element and downstream is ready or don't have to emit current element
            enabled = downsampleSequential.valid_up & \
                      ((emit_cur_element & downsampleSequential.ready_down) | (~emit_cur_element))
            # ready means can accept input when get valid from upstream
            # ready when emit current element and downstream is ready or don't have to emit current element
            ready = (emit_cur_element & downsampleSequential.ready_down) | (~emit_cur_element)
            # valid means can emit downstream
            # valid when emitting current element and upstream is providing valid input for element
            valid = emit_cur_element & downsampleSequential.valid_up

            if has_ce:
                enabled = enabled & bit(downsampleSequential.CE)
                ready = ready & bit(downsampleSequential.CE)
                valid = valid & bit(downsampleSequential.CE)

            if time_per_element > 1:
                time_per_element_counter = SizedCounterModM(time_per_element,
                                                            has_ce=True,
                                                            has_reset=has_reset)
                go_to_next_element = Decode(time_per_element - 1, time_per_element_counter.O.N)(time_per_element_counter.O)

                wire(time_per_element_counter.CE, enabled)
                wire(element_idx_counter.CE, enabled & go_to_next_element)

                if has_reset:
                    wire(time_per_element_counter.RESET, downsampleSequential.RESET)
                    wire(element_idx_counter.RESET, downsampleSequential.RESET)
            else:
                wire(element_idx_counter.CE, enabled)
                if has_reset:
                    wire(element_idx_counter.RESET, downsampleSequential.RESET)

            wire(downsampleSequential.I, downsampleSequential.O)
            wire(valid, downsampleSequential.valid_down)
            wire(ready, downsampleSequential.ready_up)

    return DownsampleSequential

def DownsampleSequential(n, time_per_element, idx, T, has_ce=False, has_reset=False):
    return DefineDownsampleSequential(n, time_per_element, idx, T, has_ce, has_reset)()
