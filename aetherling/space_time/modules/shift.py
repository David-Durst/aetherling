from aetherling.space_time import *
from aetherling.space_time.type_helpers import valid_ports
from aetherling.space_time.ram_st import DefineRAM_ST
from aetherling.modules.counter import AESizedCounterModM
from aetherling.modules.term_any_type import TermAnyType
from magma import *
from magma.circuit import DefineCircuitKind
from mantle.coreir import DefineCoreirConst
from aetherling.helpers.nameCleanup import cleanName
from aetherling.helpers.magma_helpers import ready_valid_interface

__all__ = ['DefineShift_S', 'Shift_S', 'DefineShift_T', 'Shift_T', 'DefineShift_TS', 'Shift_TS']

@cache_definition
def DefineShift_S(n: int, shift_amount: int, elem_t: ST_Type, has_valid: bool = False) -> DefineCircuitKind:
    """
    Shifts the elements in SSeq n elem_t by shift_amount to the right.

    I : In((SSeq(n, i, elem_t).magma_repr())
    O : Out((SSeq(n, i, elem_t).magma_repr())
    if has_valid:
    valid_up : In(Bit)
    valid_down : Out(Bit)
    """
    class _ShiftS(Circuit):
        name = "Shift_S_n{}_amt{}_tEl{}_hasValid{}".format(str(n), str(shift_amount), cleanName(str(elem_t)),
                                                           str(has_valid))
        binary_op = False
        st_in_t = [ST_SSeq(n, elem_t)]
        st_out_t = ST_SSeq(n, elem_t)
        IO = ['I', In(st_in_t[0].magma_repr()), 'O', Out(st_out_t.magma_repr())] + \
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

@cache_definition
def DefineShift_T(n: int, i: int, shift_amount: int, elem_t: ST_Type,
                  has_ce: bool = False, has_reset: bool = False, has_valid: bool = False) -> DefineCircuitKind:
    """
    Shifts the elements in TSeq n i elem_t by shift_amount to the right.

    I : In((TSeq(n, i, elem_t).magma_repr())
    O : Out((TSeq(n, i, elem_t).magma_repr())
    if has_ce:
    CE : In(Bit)
    if has_reset:
    RESET : In(Bit)
    if has_valid:
    valid_up : In(Bit)
    valid_down : Out(Bit)
    """
    class _ShiftT(Circuit):
        name = "Shift_t_n{}_i{}_amt{}_tEl{}__hasCE{}_hasReset{}_hasValid{}".format(str(n), str(i),
                                                                                   str(shift_amount),
                                                                                   cleanName(str(elem_t)),
                                                                                   str(has_ce),
                                                                                   str(has_reset),
                                                                                   str(has_valid))
        binary_op = False
        st_in_t = [ST_TSeq(n, i, elem_t)]
        st_out_t = ST_TSeq(n, i, elem_t)
        IO = ['I', In(st_in_t[0].magma_repr()), 'O', Out(st_out_t.magma_repr())] + \
             ClockInterface(has_ce, has_reset)
        if has_valid:
            IO += valid_ports
        @classmethod
        def definition(cls):
            enabled = DefineCoreirConst(1, 1)().O[0]
            if has_valid:
                enabled = cls.valid_up & enabled
                wire(cls.valid_up, cls.valid_down)
            if has_ce:
                enabled = bit(cls.CE) & enabled

            value_store = DefineRAM_ST(elem_t, shift_amount, has_reset=has_reset)()

            # write and read from same location
            # will write on first iteration through element, write and read on later iterations
            # output for first iteration is undefined, so ok to read anything
            next_ram_addr = DefineNestedCounters(elem_t, has_ce=True, has_reset=has_reset)()
            # its fine that this doesn't account for the invalid clocks.
            # after the invalid clocks, the next iteration will start from
            # an index that is possibly not 0. That doesn't matter
            # as will just loop around
            ram_addr = AESizedCounterModM(shift_amount, has_ce=True, has_reset=has_reset)

            wire(ram_addr.O, value_store.WADDR)
            wire(ram_addr.O, value_store.RADDR)

            wire(enabled, value_store.WE)
            wire(enabled, value_store.RE)
            wire(enabled & next_ram_addr.last, ram_addr.CE)
            wire(enabled, next_ram_addr.CE)

            next_ram_addr_term = TermAnyType(Bit)
            wire(next_ram_addr.valid, next_ram_addr_term.I)

            wire(cls.I, value_store.WDATA)
            wire(value_store.RDATA, cls.O)
            if has_reset:
                wire(value_store.RESET, cls.RESET)
                wire(ram_addr.RESET, cls.RESET)
                wire(next_ram_addr.RESET, cls.RESET)

    return _ShiftT

def Shift_T(n: int, i: int, shift_amount: int, elem_t: ST_Type,
                  has_ce: bool = False, has_reset: bool = False, has_valid: bool = False) -> Circuit:
    return DefineShift_T(n, i, shift_amount, elem_t, has_ce, has_reset, has_valid)()


"""
Shift Rewrite Rule ---
Shift (no*ni) s in_seq ===
Unpartition no ni .
    Tuple_To_Seq .
    foldl1 (\accum next -> Map2 no Tuple accum next) .
    fmap (\(i, in_seq) -> 
            Shift no ((ni - i + s - 1) // ni) $ Map no (Select_1d ni ((i - s) % ni)) in_seq
        ) .
    zipWith [0..]
    repeat ni .
    Partition no ni
"""


@cache_definition
def DefineShift_TS(no: int, io: int, ni: int, shift_amount: int, elem_t: ST_Type,
                  has_ce: bool = False, has_reset: bool = False, has_valid: bool = False) -> DefineCircuitKind:
    """
    Shifts the elements in TSeq no io (SSeq ni elem_t) by shift_amount to the right.

    I : In((TSeq(no, io, SSeq(ni, elem_t)).magma_repr())
    O : Out((TSeq(n, i, SSeq(ni, elem_t)).magma_repr())
    if has_ce:
    CE : In(Bit)
    if has_reset:
    RESET : In(Bit)
    if has_valid:
    valid_up : In(Bit)
    valid_down : Out(Bit)
    """
    class _ShiftTS(Circuit):
        name = "Shift_ts_no{}_io{}_ni{}_amt{}_tEl{}__hasCE{}_hasReset{}_hasValid{}".format(str(no), str(io),
                                                                                           str(ni),
                                                                                           str(shift_amount),
                                                                                           cleanName(str(elem_t)),
                                                                                           str(has_ce),
                                                                                           str(has_reset),
                                                                                           str(has_valid))
        binary_op = False
        st_in_t = [ST_TSeq(no, io, ST_SSeq(ni, elem_t))]
        st_out_t = ST_TSeq(no, io, ST_SSeq(ni, elem_t))
        IO = ['I', In(st_in_t[0].magma_repr()), 'O', Out(st_out_t.magma_repr())] + \
             ClockInterface(has_ce, has_reset)
        if has_valid:
            IO += valid_ports
        @classmethod
        def definition(cls):
            enabled = DefineCoreirConst(1, 1)().O[0]
            if has_valid:
                enabled = cls.valid_up & enabled
                wire(cls.valid_up, cls.valid_down)
            if has_ce:
                enabled = bit(cls.CE) & enabled

            # don't need valid on these shift_t as they'll be getting it from the enable signal
            shift_t_xs = []
            for i in range(ni):
                shift_amount_t = (ni - i + shift_amount - 1) // ni
                if shift_amount_t == 0:
                    shift_t_xs.append(None)
                else:
                    shift_t_xs.append(DefineShift_T(no, io, shift_amount_t, elem_t, True, has_reset, False)())

            for i in range(ni):
                if shift_t_xs[i] is None:
                    wire(cls.I[(i - shift_amount) % ni], cls.O[i])
                else:
                    wire(cls.I[(i - shift_amount) % ni], shift_t_xs[i].I)
                    wire(shift_t_xs[i].O, cls.O[i])
                    wire(enabled, shift_t_xs[i].CE)
                    if has_reset:
                        wire(cls.RESET, shift_t_xs[i].RESET)

    return _ShiftTS

def Shift_TS(no: int, io: int, ni: int, shift_amount: int, elem_t: ST_Type,
            has_ce: bool = False, has_reset: bool = False, has_valid: bool = False) -> Circuit:
    return DefineShift_T(no, io, ni, shift_amount, elem_t, has_ce, has_reset, has_valid)()
