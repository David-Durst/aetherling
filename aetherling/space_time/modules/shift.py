from aetherling.space_time import *
from aetherling.space_time.type_helpers import valid_ports
from mantle.common.countermod import SizedCounterModM
from aetherling.modules.ram_any_type import DefineRAMAnyType
from aetherling.modules.register_any_type import DefineRegisterAnyType
from magma import *
from magma.circuit import DefineCircuitKind
from aetherling.helpers.nameCleanup import cleanName
from aetherling.helpers.magma_helpers import ready_valid_interface

#__all__ = ['DefineShift_S', 'Shift_S', 'DefineShift_T', 'RShift_T']

@cache_definition
def DefineShift_S(n: int, shift_amount: int, elem_t: ST_Type, has_valid: bool = False) -> DefineCircuitKind:
    """
    Shifts the elements in SSeq n T' by shift_amount to the right.
    THe first shift_amount elements are undefined.

    I : In(Array[n, T])
    O : Out(Array[n, T])
    if has_valid:
    valid_up : In(Bit)
    valid_down : Out(Bit)
    """
    class _ShiftS(Circuit):
        name = "Shift_S_n{}_amt{}_t{}_v{}".format(str(n), str(shift_amount), cleanName(str(elem_t)),
                                                                        str(has_valid))
        IO = ['I', In(ST_SSeq(n, elem_t).magma_repr()), 'O', Out(ST_SSeq(n, elem_t).magma_repr())] + \
             ClockInterface(False, False)
        if has_valid:
            IO += valid_ports
        @classmethod
        def definition(cls):
            for i in range(n):
                # wrap around. first shift_amount outputs undefined, so anything can go out there
                wire(cls.I[i], cls.O[(i + shift_amount) % n])
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _ShiftS

def Shift_S(n: int, shift_amount: int, elem_t: ST_Type, has_valid: bool = False) -> Circuit:
    return DefineShift_S(n, shift_amount, elem_t, has_valid)()
"""
@cache_definition
def DefineRShiftSequential(n: int, time_per_element: int, shift_amount: int, T: Kind, has_ce=False, has_reset=False):
    " ""
    Produces the bank  the elements in TSeq n T' by shift_amount to the right.
    The first shift_amount elements are undefined

    Each T period is time_per_element clock cycles
    You can get time_per_element by calling time on a space-time type.

    Note that the T passed to this operator just the Magma type each clock cycle.
    You can get T by calling magma_repr on a space-time type T'.

    I : In(Array[n, T])
    O : Out(Array[n, T])
    if has_ready_valid:
    ready_up : Out(Bit)
    valid_up : In(Bit)
    ready_down : In(Bit)
    valid_down : Out(Bit)
    " ""
    class _RShiftSequential(Circuit):
        name = "RShiftSequential_n{}_tEl{}_amt{}_T{}_hasCE{}_hasReset{}".format(str(n), str(time_per_element),
                                                                                       str(shift_amount),
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

            if shift_amount > 1 or time_per_element > 1:
                value_store = DefineRAMAnyType(T, shift_amount * time_per_element)()
                value_store_input = value_store.WDATA
                value_store_output = value_store.RDATA

                # write and read from same location
                # will write on first iteration through element, write and read on later iterations
                # output for first iteration is undefined, so ok to read anything
                ram_addr = SizedCounterModM(shift_amount * time_per_element, has_ce=True, has_reset=has_reset)
                wire(ram_addr.O, value_store.WADDR)
                wire(ram_addr.O, value_store.RADDR)
                wire(enabled, value_store.WE)
                wire(enabled, ram_addr.CE)
            else:
                value_store = DefineRegisterAnyType(T, has_ce=True)()
                value_store_input = value_store.I
                value_store_output = value_store.O

                wire(value_store.CE, enabled)

            wire(rshiftSequential.I, value_store_input)
            wire(value_store_output, rshiftSequential.O)

            wire(valid, rshiftSequential.valid_down)
            wire(ready, rshiftSequential.ready_up)
    return _RShiftSequential

def RShiftSequential(n: int, time_per_element: int, shift_amount: int, T: Kind, has_ce=False, has_reset=False):
    return DefineRShiftSequential(n, time_per_element, shift_amount, T, has_ce, has_reset)()
"""
