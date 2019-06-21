import math

from mantle import Decode
from mantle.common.countermod import SizedCounterModM
from magma import *
from ..helpers.nameCleanup import cleanName
from ..helpers.magma_helpers import ready_valid_interface
from aetherling.modules.register_any_type import DefineRegisterAnyType
from aetherling.modules.mux_any_type import DefineMuxAnyType
from aetherling.modules.term_any_type import DefineTermAnyType
from aetherling.modules.ram_any_type import DefineRAMAnyType

__all__ = ['DefineUpsampleParallel', 'UpsampleParallel', 'DefineUpsampleSequential', 'UpsampleSequential']

@cache_definition
def DefineUpsampleParallel(n, T, has_ready_valid=False):
    """
    Upsample a SSeq 1 T' to and SSeq n T' in one period.

    The time_per_element clock cycles in a period is not relevant for this operator as it is combinational.

    Note that the T passed to this operator just the Magma type each clock cycle.
    You can get T by calling magma_repr on a space-time type T'.

    I : In(T)
    O : Out(Array[n, T])

    if has_ready_valid:
    ready_up : Out(Bit)
    valid_up : In(Bit)
    ready_down : In(Bit)
    valid_down : Out(Bit)
    """
    class UpParallel(Circuit):
        name = "UpsampleParallel_n{}_T{}_rv{}".format(str(n), cleanName(str(T)), str(has_ready_valid))
        IO = ['I', In(T), 'O', Out(Array[n, T])]
        if has_ready_valid:
            IO += ready_valid_interface
        @classmethod
        def definition(upsampleParallel):
            for i in range(n):
                upsampleParallel.wire = wire(upsampleParallel.I, upsampleParallel.O[i])
            if has_ready_valid:
                wire(upsampleParallel.ready_up, upsampleParallel.ready_down)
                wire(upsampleParallel.valid_up, upsampleParallel.valid_down)
    return UpParallel

def UpsampleParallel(n, T, has_ready_valid=False):
    return DefineUpsampleParallel(n, T, has_ready_valid)()

@cache_definition
def DefineUpsampleSequential(n, time_per_element, T, has_ce=False, has_reset=False):
    """
    Upsample a TSeq 1 T' to a TSeq n T' over n periods.

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
    class UpSequential(Circuit):
        name = "UpsampleSequential_n{}_tEl{}_T{}_hasCE{}_hasReset{}".format(str(n), str(time_per_element), \
                                                                            cleanName(str(T)), str(has_ce), str(has_reset))
        IO = ['I', In(T), 'O', Out(T)] + ClockInterface(has_ce, has_reset) + ready_valid_interface
        @classmethod
        def definition(upsampleSequential):
            enabled = upsampleSequential.ready_down & upsampleSequential.valid_up
            if has_ce:
                enabled = enabled & bit(upsampleSequential.CE)

            # the counter of the current element, when hits 0, load the next input to upsample
            element_idx_counter = SizedCounterModM(n, has_ce=True, has_reset=has_reset)
            is_first_element = Decode(0, element_idx_counter.O.N)(element_idx_counter.O)
            if has_reset:
                wire(upsampleSequential.RESET, element_idx_counter.RESET)

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

            # on first clock cycle, send the input directly out. otherwise, use the register
            wire(is_first_element, output_selector.sel[0])
            wire(value_store_output, output_selector.data[0])
            wire(upsampleSequential.I, output_selector.data[1])
            wire(output_selector.out, upsampleSequential.O)

            wire(enabled, upsampleSequential.valid_down)
            wire(enabled & is_first_element, upsampleSequential.ready_up)

    return UpSequential

def UpsampleSequential(n, time_per_element, T, has_ce=False, has_reset=False):
    return DefineUpsampleSequential(n, time_per_element, T, has_ce, has_reset)()

"""
from coreir.context import *
from magma.backend.coreir_ import CoreIRBackend
from magma.coreirModuleWrapper import ModuleFromGeneratorWrapper
from magma import *
c = Context()
cirb = CoreIRBackend(c)
x = ModuleFromGeneratorWrapper(cirb, "aetherlinglib", "dehydrate", {"hydratedType": cirb.get_type(Array[3, Array[5, BitIn])], True)})
dehydrate = cirb.context.import_generator("aetherlinglib", "dehydrate")(hydratedType = cirb.get_type(Array[3, Array[5, BitIn])], True))
cirb.context.give_coreir_module_definition(dehydrate)

"""