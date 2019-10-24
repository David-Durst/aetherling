from magma import *
from magma.circuit import DefineCircuitKind
from magma.frontend.coreir_ import CircuitInstanceFromGeneratorWrapper, GetCoreIRBackend
from aetherling.helpers.nameCleanup import cleanName
from magma.t import Kind
from aetherling.modules.term_any_type import DefineTermAnyType
from aetherling.space_time.type_helpers import valid_ports
from aetherling.modules import DefineRAMAnyType, DefineRegisterAnyType, DefineMuxAnyType, DefineNativeMapParallel
from mantle.common.countermod import SizedCounterModM
from mantle.common.decode import Decode
from mantle.coreir import DefineSub
from mantle.coreir import DefineCoreirConst

@cache_definition
def DefineSerializer(n:int, i:int, T: Kind, has_reset=False) -> DefineCircuitKind:
    """
    Convert sequences in space to sequences in time.
    TSeq 1 (n-1+i) (SSeq n T) -> TSeq n i T
    Each T must only be in space, not time

    Note that the T passed to this operator just the Magma type emitted each clock cycle.
    You can get T by calling magma_repr on a space-time type T'.

    I : In(Array[n, T])
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
        name = "serialize_t{}_tEl{}_n{}_hasCE{}_hasRESET{}".format(cleanName(str(T)), str(n), \
                                                                   str(i), str(has_reset))

        IO = ["I", In(Array[n, T]), "O", Out(T)] + \
             ClockInterface(has_ce=False, has_reset=has_reset) + valid_ports

        @classmethod
        def definition(cls):
            # the counter of the current element of output sequence, when hits 0, load the next input to serialize
            element_idx_counter = SizedCounterModM(n+i, has_ce=True, has_reset=has_reset)
            is_first_element = Decode(0, element_idx_counter.O.N)(element_idx_counter.O)

            wire(cls.valid_up, cls.valid_down)

            if has_reset:
                wire(cls.RESET, element_idx_counter.RESET)

            value_store = DefineNativeMapParallel(n-1, DefineRegisterAnyType(T, has_ce=True))()
            value_store_input = value_store.I
            value_store_output = value_store.O

            wire(element_idx_counter.CE, cls.valid_up)
            for input_idx in range(n):
                wire(value_store.CE[input_idx], is_first_element & cls.valid_up)

            wire(cls.I, value_store_input)

            # to serialize, go through all different rams/registers in value store
            # and select the output from the ith one, where i is current output element
            value_store_output_selector = DefineMuxAnyType(T, n-1)()
            wire(value_store_output, value_store_output_selector.data)
            sub1 = DefineSub(element_idx_counter.O.N)()
            wire(sub1.I0, element_idx_counter.O)
            one_const = DefineCoreirConst(element_idx_counter.O.N, 1)
            wire(sub1.I1, one_const.O)
            if element_idx_counter.O.N == value_store_output_selector.sel.N:
                wire(sub1.O, value_store_output_selector.sel)
            else:
                used_bits_length = value_store_output_selector.sel.N
                unused_bits_length = sub1.O.N - used_bits_length
                wire(sub1.O[:used_bits_length-1], value_store_output_selector.sel)
                term = DefineTermAnyType(Array[unused_bits_length, Bit])
                wire(sub1.O[used_bits_length:], term.I)


            # on first element, send the input directly out. otherwise, use the register
            first_element_output_selector = DefineMuxAnyType(T, 2)()
            wire(is_first_element, first_element_output_selector.sel[0])
            wire(value_store_output_selector, first_element_output_selector.data[0])
            wire(cls.I[0], first_element_output_selector.data[1])
            wire(first_element_output_selector.out, cls.O)

    return _Serializer

def Serializer(n:int, time_per_element: int, T: Kind, has_ce=False, has_reset=False) -> Circuit:
    return DefineSerializer(n, time_per_element, T, has_ce, has_reset)()
