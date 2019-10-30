from magma import *
from magma.circuit import DefineCircuitKind
from magma.frontend.coreir_ import CircuitInstanceFromGeneratorWrapper, GetCoreIRBackend
from aetherling.helpers.nameCleanup import cleanName
from magma.t import Kind
from aetherling.modules.term_any_type import DefineTermAnyType
from aetherling.space_time.type_helpers import *
from aetherling.space_time.ram_st import DefineRAM_ST
from aetherling.modules import DefineRAMAnyType, DefineRegisterAnyType, DefineMuxAnyType, DefineNativeMapParallel
from mantle.common.countermod import SizedCounterModM
from mantle.common.register import DefineRegister
from mantle.coreir.memory import DefineRAM
from mantle.common.decode import Decode
from mantle.coreir import DefineSub
import math

@cache_definition
def DefineSerialize(n:int, i_:int, T: ST_Type, has_reset=False) -> DefineCircuitKind:
    """
    Convert sequences in space to sequences in time.
    TSeq no (vo+no*vi) (SSeq n T') -> TSeq no vo (TSeq n vi T')

    Each T' period is time_per_element clock cycles
    You can get time_per_element by calling time on a space-time type.

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
    class _Serialize(Circuit):
        name = "serialize_t{}_n{}_i{}_hasRESET{}".format(cleanName(str(T)), \
                                                                   str(n), str(i_), str(has_reset))
        st_in_t = [ST_TSeq(1, n-1+i_, ST_SSeq_Tuple(n, T))]
        st_out_t = ST_TSeq(n, i_, T)
        binary_op=False
        IO = ["I", In(st_in_t[0].magma_repr()), "O", Out(st_out_t.magma_repr())] + \
             ClockInterface(False, has_reset) + valid_ports

        @classmethod
        def definition(cls):
            # the counter of the current element of output sequence, when hits 0, load the next input to serialize
            element_idx_counter = SizedCounterModM(n + i_, has_ce=True, has_reset=has_reset)
            if element_idx_counter.O.N == math.ceil(math.log(n, 2)):
                element_idx_out = element_idx_counter.O
            else:
                used_bits_length = (math.ceil(math.log(n, 2)))
                unused_bits_length = element_idx_counter.O.N - used_bits_length
                element_idx_out = element_idx_counter.O[:used_bits_length]
                term = DefineTermAnyType(Array[unused_bits_length, Bit])()
                wire(element_idx_counter.O[unused_bits_length:], term.I)
            is_first_element = Decode(0, element_idx_out.N)(element_idx_out)

            enabled = cls.valid_up
            valid_reg = DefineRegister(1)()
            wire(cls.valid_up, valid_reg.I[0])
            wire(valid_reg.O[0], cls.valid_down)

            # if each element takes multiple clocks, need a ram so can write all them and read them over multiple clocks
            if is_nested(T) and T.time() > 1:
                value_store = [DefineRAMAnyType(T.magma_repr(), T.time())() for _ in range(n-1)]
                value_store_input = [ram.WDATA for ram in value_store]
                value_store_output = [ram.RDATA for ram in value_store]

                time_per_element_counter = SizedCounterModM(T.time(),
                                                            has_ce=True, has_reset=has_reset)
                go_to_next_element = Decode(T.time() - 1, time_per_element_counter.O.N)(time_per_element_counter.O)

                wire(time_per_element_counter.CE, enabled)
                wire(element_idx_counter.CE, enabled & go_to_next_element)
                for input_idx in range(n-1):
                    wire(value_store[input_idx].WE, is_first_element & enabled)
                    # location in current element is where to read and write.
                    # will write on first iteration through element, read on later iterations
                    wire(time_per_element_counter.O, value_store[input_idx].WADDR)
                    wire(time_per_element_counter.O, value_store[input_idx].RADDR)

                if has_reset:
                    wire(time_per_element_counter.RESET, cls.RESET)

            else:
                value_store = [DefineRegisterAnyType(T.magma_repr(), has_ce=True)() for _ in range(n-1)]
                value_store_input = [reg.I for reg in value_store]
                value_store_output = [reg.O for reg in value_store]

                wire(element_idx_counter.CE, enabled)
                for input_idx in range(n-1):
                    wire(value_store[input_idx].CE, is_first_element & enabled)


            for i in range(n-1):
                wire(cls.I[i+1], value_store_input[i])

            # to serialize, go through all different rams/registers in value store
            # and select the output from the ith one, where i is current output element
            value_store_output_selector = DefineMuxAnyType(T.magma_repr(), n)()
            for i in range(n-1):
                wire(value_store_output[i], value_store_output_selector.data[i+1])
            # just wiring this up to avoid any issues
            wire(value_store_output[0], value_store_output_selector.data[0])
            wire(element_idx_out, value_store_output_selector.sel)

            # on first element, send the input directly out. otherwise, use the register
            first_element_output_selector = DefineMuxAnyType(T.magma_repr(), 2)()
            wire(is_first_element, first_element_output_selector.sel[0])
            wire(value_store_output_selector.out, first_element_output_selector.data[0])
            wire(cls.I[0], first_element_output_selector.data[1])
            out_reg = DefineRegisterAnyType(cls.st_out_t.magma_repr())()
            wire(first_element_output_selector.out, out_reg.I)
            wire(out_reg.O, cls.O)

    return _Serialize

def Serialize(n:int, time_per_element: int, T: Kind, has_ce=False, has_reset=False) -> Circuit:
    return DefineSerialize(n, time_per_element, T, has_ce, has_reset)()
