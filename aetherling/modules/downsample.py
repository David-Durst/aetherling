import math

from mantle import Register, Decode
from mantle.common.countermod import SizedCounterModM
from mantle.common.operator import *
from aetherling.modules.term_any_type import TermAnyType
from magma import *
from .hydrate import Dehydrate, Hydrate
from .map_fully_parallel_sequential import MapParallel
from ..helpers.nameCleanup import cleanName
from ..helpers.magma_helpers import ready_valid_interface

__all__ = ['DefineDownsampleParallel', 'DownsampleParallel',
           'DefineDownsampleSequential', 'DownsampleSequential']

@cache_definition
def DefineDownsampleParallel(n, idx, T, has_ready_valid=False):
    """
    Downsample an array of T's to a single T in one period.
    idx specifies which element of the downsample

    I : In(Array[n, T])
    O : Out(T)
    if has_ready_valid:
    ready_up : Out(Bit)
    valid_up : In(Bit)
    ready_down : In(Bit)
    valid_down : Out(Bit)
    """
    class DownsampleParallel(Circuit):
        name = "DownsampleParallel_n{}_T{}".format(str(n), cleanName(str(T)))
        IO = ['I', In(Array[n, T]), 'O', Out(T)]
        if has_ready_valid:
            IO += ready_valid_interface
        @classmethod
        def definition(downsampleParallel):
            # dehydrate all but the first, that one is passed through
            inputs_term = TermAnyType(Array[n-1, T])
            num_wired_to_output = 0
            for i in range(len(downsampleParallel.I)):
                if i == idx:
                    wire(downsampleParallel.I[i], downsampleParallel.O)
                    num_wired_to_output += 1
                else:
                    wire(downsampleParallel.I[i], inputs_term.I[i - num_wired_to_output])
            if has_ready_valid:
                wire(downsampleParallel.ready_up, downsampleParallel.ready_down)
                wire(downsampleParallel.valid_up, downsampleParallel.valid_down)
    return DownsampleParallel

def DownsampleParallel(n, idx, T, has_ready_valid=False):
    return DefineDownsampleParallel(n, idx, T, has_ready_valid)()

@cache_definition
def DefineDownsampleSequential(n, time_per_element, idx, T, has_ce=False, has_reset=False):
    """
    Downsample a stream of n T's to a single T over n*time_per_element clock cycles.
    Each T should take time_per_element clock cycles
    VALID is asserted on the clock cycle when valid output is coming out
    Aetherling Type: {n, T} -> {1, T}

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
        name = "DownsampleSequential_n{}_T{}_hasCE{}_hasReset{}".format(str(n), \
               cleanName(str(T)), str(has_ce), str(has_reset))
        IO = ['I', In(T), 'O', Out(T)] + ClockInterface(has_ce, has_reset) + ready_valid_interface
        @classmethod
        def definition(downsampleSequential):
            enabled = downsampleSequential.ready_down & downsampleSequential.valid_up
            if has_ce:
                enabled = enabled & bit(downsampleSequential.CE)

            if time_per_element > 1:
                time_per_element_counter = SizedCounterModM(time_per_element,
                                                            has_ce=True,
                                                            has_reset=has_reset)
                go_to_next_element = Decode(time_per_element - 1, time_per_element_counter.O.N)(time_per_element_counter.O)
                element_idx_counter = SizedCounterModM(n, has_ce=True, has_reset=has_reset)

                wire(time_per_element_counter.CE, enabled)
                wire(element_idx_counter.CE, enabled & go_to_next_element)

                if has_reset:
                    wire(time_per_element_counter.RESET, downsampleSequential.RESET)
                    wire(element_idx_counter.RESET, downsampleSequential.RESET)
            else:
                element_idx_counter = SizedCounterModM(n, has_ce=True, has_reset=has_reset)
                wire(element_idx_counter.CE, enabled)
                if has_reset:
                    wire(element_idx_counter.RESET, downsampleSequential.RESET)

            emit_cur_element = Decode(idx, element_idx_counter.O.N)(element_idx_counter.O)
            wire(downsampleSequential.I, downsampleSequential.O)
            # on first clock cycle, send the input directly out. otherwise, use the register
            wire(emit_cur_element & enabled, downsampleSequential.valid_down)
            wire(downsampleSequential.ready_down, downsampleSequential.ready_up)

    return DownsampleSequential

def DownsampleSequential(n, time_per_element, idx, T, has_ce=False, has_reset=False):
    return DefineDownsampleSequential(n, time_per_element, idx, T, has_ce, has_reset)()
