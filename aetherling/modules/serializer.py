from magma import *
from magma.circuit import DefineCircuitKind
from magma.frontend.coreir_ import CircuitInstanceFromGeneratorWrapper, GetCoreIRBackend
from ..helpers.nameCleanup import cleanName
from magma.t import Kind
from mantle.coreir.type_helpers import Term
from .hydrate import DefineDehydrate, Dehydrate, DefineHydrate, Hydrate
from .map_fully_parallel_sequential import MapParallel
from aetherling.helpers.magma_helpers import ready_valid_interface
from aetherling.modules import DefineRAMAnyType, DefineRegisterAnyType, DefineMuxAnyType, DefineNativeMapParallel
from mantle.common.countermod import SizedCounterModM
from mantle.common.decode import Decode

@cache_definition
def DefineSerializer(n:int, time_per_element: int, T: Kind, has_ce=False, has_reset=False) -> DefineCircuitKind:
    """
    Convert sequences in space to sequences in time.
    TSeq no (vo+no*vi) (SSeq n T') -> TSeq no vo (TSeq n vi T')

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
    class _Serializer(Circuit):
        name = "serialize_t{}_tEl{}_n{}_hasCE{}_hasRESET{}".format(cleanName(str(T)), str(time_per_element), \
                                                                   str(n), str(has_ce), str(has_reset))

        IO = ["I", In(Array[n, T]), "O", Out(T)] + \
             ClockInterface(has_ce, has_reset) + ready_valid_interface

        @classmethod
        def definition(serializer):
            enabled = serializer.ready_down & serializer.valid_up
            if has_ce:
                enabled = enabled & bit(serializer.CE)

            # the counter of the current element of output sequence, when hits 0, load the next input to serialize
            element_idx_counter = SizedCounterModM(n, has_ce=True, has_reset=has_reset)
            is_first_element = Decode(0, element_idx_counter.O.N)(element_idx_counter.O)
            if has_reset:
                wire(serializer.RESET, element_idx_counter.RESET)

            # if each element takes multiple clocks, need a ram so can write all them and read them over multiple clocks
            if time_per_element > 1:
                value_store = DefineNativeMapParallel(n, DefineRAMAnyType(T, time_per_element))()
                value_store_input = value_store.WDATA
                value_store_output = value_store.RDATA

                time_per_element_counter = SizedCounterModM(time_per_element,
                                                            has_ce=True, has_reset=has_reset)
                go_to_next_element = Decode(time_per_element - 1, time_per_element_counter.O.N)(time_per_element_counter.O)

                wire(time_per_element_counter.CE, enabled)
                wire(element_idx_counter.CE, enabled & go_to_next_element)
                for input_idx in range(n):
                    wire(value_store[input_idx].WE, is_first_element & enabled)
                    # location in current element is where to read and write.
                    # will write on first iteration through element, read on later iterations
                    wire(time_per_element_counter.O, value_store[input_idx].WADDR)
                    wire(time_per_element_counter.O, value_store[input_idx].RADDR)

                if has_reset:
                    wire(time_per_element_counter.RESET, serializer.RESET)

            else:
                value_store = DefineNativeMapParallel(n, DefineRegisterAnyType(T, has_ce=True))()
                value_store_input = value_store.I
                value_store_output = value_store.O

                wire(element_idx_counter.CE, enabled)
                for input_idx in range(n):
                    wire(value_store.CE, is_first_element & enabled)


            for input_idx in range(n):
                wire(serializer.I, value_store_input[input_idx])

            # to serialize, go through all different rams/registers in value store
            # and select the output from the ith one, where i is current output element
            value_store_output_selector = DefineMuxAnyType(T, n)()
            wire(value_store_output, value_store_output_selector.data)
            wire(element_idx_counter.O, value_store_output_selector.sel)

            # on first element, send the input directly out. otherwise, use the register
            first_element_output_selector = DefineMuxAnyType(T, 2)()
            wire(is_first_element, first_element_output_selector.sel[0])
            wire(value_store_output_selector, first_element_output_selector.data[0])
            wire(serializer.I, first_element_output_selector.data[1])
            wire(first_element_output_selector.out, serializer.O)

            wire(enabled, serializer.valid_down)
            wire(enabled & is_first_element, serializer.ready_up)

    return _Serializer

def Serializer(n:int, time_per_element: int, T: Kind, has_ce=False, has_reset=False) -> Circuit:
    return DefineSerializer(n, time_per_element, T, has_ce, has_reset)()

@cache_definition
def DefineDeserializer(n: int, time_per_element: int, T: Kind, has_ce=False, has_reset=False) -> DefineCircuitKind:
    """
    Convert sequences in time to sequences in space.
    TSeq no vo (TSeq n vi T') -> TSeq no (vo+no*vi) (SSeq n T')

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

    class _Deserializer(Circuit):
        name = "deserialize_t{}_tEl{}_n{}_hasCE{}_hasRESET{}".format(cleanName(str(T)), str(time_per_element), \
                                                                     str(n), str(has_ce), str(has_reset))

        IO = ["I", In(Array[n, T]), "out", Out(T)] + \
             ClockInterface(has_ce, has_reset) + ready_valid_interface

        @classmethod
        def definition(deserializer):
            enabled = deserializer.ready_down & deserializer.valid_up
            if has_ce:
                enabled = enabled & bit(deserializer.CE)

            # the counter of the current element of output sequence, when hits 0, load the next input to serialize
            element_idx_counter = SizedCounterModM(n, has_ce=True, has_reset=has_reset)
            is_last_element = Decode(n-1, element_idx_counter.O.N)(element_idx_counter.O)
            if has_reset:
                wire(deserializer.RESET, element_idx_counter.RESET)

            # if each element takes multiple clocks, need a ram so can write all them and read them over multiple clocks
            if time_per_element > 1:
                # only use n-1 value store, just wire nth input directly to output since outputting whole
                # deserialized sequence on period receiving nth input
                value_store = DefineNativeMapParallel(n-1, DefineRAMAnyType(T, time_per_element))()
                value_store_input = value_store.WDATA
                value_store_output = value_store.RDATA
                value_store_enables = value_store.CE

                time_per_element_counter = SizedCounterModM(time_per_element,
                                                            has_ce=True, has_reset=has_reset)
                go_to_next_element = Decode(time_per_element - 1, time_per_element_counter.O.N)(time_per_element_counter.O)

                wire(time_per_element_counter.CE, enabled)
                wire(element_idx_counter.CE, enabled & go_to_next_element)
                for input_idx in range(n-1):
                    # location in current element is where to read and write.
                    # will write on first iteration through each element, read on last iteration from all elements
                    wire(time_per_element_counter.O, value_store[input_idx].WADDR)
                    wire(time_per_element_counter.O, value_store[input_idx].RADDR)

                if has_reset:
                    wire(time_per_element_counter.RESET, deserializer.RESET)

            else:
                value_store = DefineNativeMapParallel(n-1, DefineRegisterAnyType(T, has_ce=True))()
                value_store_input = value_store.I
                value_store_output = value_store.O
                value_store_enables = value_store.CE
                wire(element_idx_counter.CE, enabled)

            for element_idx in range(n-1):
                # send input to all value stores
                # the enables will ensure only the right store each period reads in the value
                wire(deserializer.I, value_store_input[element_idx])
                # to deserialize, enable the ith rams/registers in value store
                # for ith element input
                idx_match_cur_element = Decode(element_idx, element_idx_counter.O.N)(element_idx_counter.O)
                wire(enabled & idx_match_cur_element, value_store_enables[element_idx])
                wire(value_store_output[element_idx], deserializer.O[element_idx])

            # send the last input directly out
            wire(deserializer.I[n-1], deserializer.O[n-1])

            wire(enabled & is_last_element, deserializer.valid_down)
            wire(enabled, deserializer.ready_up)

    return _Deserializer

def Deserializer(n: int, time_per_element: int, T: Kind, has_ce=False, has_reset=False) -> Circuit:
    return DefineDeserializer(n, time_per_element, T, has_ce, has_reset)()