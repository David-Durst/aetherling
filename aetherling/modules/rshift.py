from mantle import Register, Decode
from mantle.common.arith import UMod
from mantle.common.countermod import SizedCounterModM
from mantle.common.operator import *
from aetherling.modules.term_any_type import TermAnyType
from aetherling.modules.ram_any_type import DefineRAMAnyType
from aetherling.modules.register_any_type import DefineRegisterAnyType
from aetherling.modules.mux_any_type import DefineMuxAnyType
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
        name = "RShiftParallel_n{}_init{}_amt{}_T_rv{}".format(str(n), init.name,
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
def DefineRShiftSequential(n: int, time_per_element: int, init: DefineCircuitKind,
                           shift_amount: int, T: Kind, has_ce=False, has_reset=False):
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
    class _RShiftSequential(Circuit):
        name = "RShiftSequential_n{}_tEl{}_init{}_amt{}_T{}_hasCE{}_hasReset{}".format(str(n), str(time_per_element),
                                                                                       init.name, str(shift_amount),
                                                                                       cleanName(str(T)), str(has_ce), str(has_reset))
        IO = ['I', In(T), 'O', Out(T)] + ClockInterface(has_ce, has_reset) + ready_valid_interface
        @classmethod
        def definition(rshiftSequential):
            # ready means can accept input
            # ready only when downstream - since constant rate module, only accept
            # when the downstream can accept as well
            # do this when in first element or downstream ready to accept
            ready = rshiftSequential.ready_down
            # valid means can emit downstream
            # valid upstream is valid as this is a constant rate module, just forwarding
            # from upstream
            valid = rshiftSequential.valid_up
            # only run when downstream is ready to accept and upstream is emitting valid data
            enabled = ready & valid

            # only assert these signals when CE is high or no CE
            if has_ce:
                enabled = enabled & bit(rshiftSequential.CE)
                ready = ready & bit(rshiftSequential.CE)
                valid = valid & bit(rshiftSequential.CE)

            # track all the inputs so know when to switch from constant to shift memory
            num_inputs_counter = SizedCounterModM(n * time_per_element,
                                                      has_ce=True, has_reset=has_reset)
            emit_from_shifter = num_inputs_counter.O >= (shift_amount * time_per_element)
            wire(num_inputs_counter.CE, enabled)

            if has_reset:
                wire(rshiftSequential.RESET, num_inputs_counter.RESET)

            if shift_amount > 1:
                value_store = DefineRAMAnyType(T, shift_amount * time_per_element)()
                value_store_input = value_store.WDATA
                value_store_output = value_store.RDATA

                # write and read from num_inputs_counter % shift_amount * time_per_element location
                # the RAM addressd space is 0 to that, and writer is always shift_amount * time_per_element ahead
                # will write on first iteration through element, write and read on later iterations
                ram_address = UMod(num_inputs_counter.O.N)(num_inputs_counter.O, shift_amount * time_per_element)
                wire(ram_address, value_store.WADDR)
                wire(ram_address, value_store.RADDR)
                wire(enabled, value_store.WE)


            else:
                value_store = DefineRegisterAnyType(T, has_ce=True)()
                value_store_input = value_store.I
                value_store_output = value_store.O

                wire(value_store.CE, enabled)

            output_selector = DefineMuxAnyType(T, 2)()

            wire(rshiftSequential.I, value_store_input)

            # on first shift_amount elements, send the input directly out. otherwise, use the register
            wire(emit_from_shifter, output_selector.sel[0])
            wire(value_store_output, output_selector.data[1])
            wire(init().O, output_selector.data[0])
            wire(output_selector.out, rshiftSequential.O)

            wire(valid, rshiftSequential.valid_down)
            wire(ready, rshiftSequential.ready_up)
    return _RShiftSequential

def RShiftSequential(n: int, time_per_element: int, init: DefineCircuitKind,
                     shift_amount: int, T: Kind, has_ce=False, has_reset=False):
    return DefineRShiftSequential(n, time_per_element, init, shift_amount, T, has_ce, has_reset)()
