from aetherling.space_time.space_time_types import *
from aetherling.space_time.type_helpers import ST_Tombstone, is_nested
from aetherling.modules.term_any_type import TermAnyType
from mantle.coreir import DefineCoreirConst, DefineCoreirUlt
from mantle.coreir.memory import getRAMAddrWidth
from mantle.common.countermod import Decode
from aetherling.modules.ram_any_type import *
from aetherling.helpers.nameCleanup import cleanName
from aetherling.modules.counter import AESizedCounterModM
from magma import *
from magma.circuit import DefineCircuitKind, Circuit
from typing import List
import math

__all__ = ['DefineNestedCounters', 'NestedCounters']

@cache_definition
def DefineNestedCounters(t: ST_Type, has_last: bool = True, has_cur_valid: bool = False,
                         has_ce: bool = False, has_reset: bool = False) \
            -> DefineCircuitKind:
    """
    Generate a set of nested counters that emit valid according to the specified type t.
    Last indicates whether this is the last clock (not period) of the type
    has_cur_valid indicates whether this counter should also emit the current valid index among all the valids across
    the nested types.

    valid: Out(Bit)
    last: Out(Bit)
    if has_cur_valid
    cur_valid: Out(Array[getRAMAddrWidth(t.valid_clocks()), Bit])
    if has_ce:
    CE : In(Bit)
    if has_reset:
    RESET : In(Bit)
    """

    class _NestedCounters(Circuit):
        name = 'NestedCounters_{}_hasCE{}_hasReset{}'.format(cleanName(str(t)), str(has_ce), str(has_reset))
        IO = ['valid', Out(Bit)] + ClockInterface(has_ce=has_ce, has_reset=has_reset)
        if has_last:
            IO += ['last', Out(Bit)]
        if has_cur_valid:
            IO += ['cur_valid', Out(Array[getRAMAddrWidth(t.valid_clocks()), Bit])]
        @classmethod
        def definition(cls):
            if type(t) == ST_TSeq:
                outer_counter = AESizedCounterModM(t.n + t.i, has_ce=True, has_reset=has_reset)
                inner_counters = DefineNestedCounters(t.t, has_last=True, has_cur_valid=False,
                                                      has_ce=has_ce, has_reset=has_reset)()
                if has_last:
                    is_last = Decode(t.n + t.i - 1, outer_counter.O.N)(outer_counter.O)
                if has_cur_valid:
                    cur_valid_counter = AESizedCounterModM(t.valid_clocks(), has_ce=True, has_reset=has_reset)
                    wire(cur_valid_counter.O, cls.cur_valid)

                # if t.n is a power of 2 and always valid, then outer_counter.O.N not enough bits
                # for valid_length to contain t.n and for is_valid to get the right input
                # always valid in this case, so just emit 1
                if math.pow(2, outer_counter.O.N) - 1 < t.n:
                    is_valid = DefineCoreirConst(1, 1)().O[0]
                    if not has_last:
                        # never using the outer_counter is not has_last
                        last_term = TermAnyType(type(outer_counter.O))
                        wire(outer_counter.O, last_term.I)
                else:
                    valid_length = DefineCoreirConst(outer_counter.O.N, t.n)()
                    is_valid_cmp = DefineCoreirUlt(outer_counter.O.N)()
                    wire(is_valid_cmp.I0, outer_counter.O)
                    wire(is_valid_cmp.I1, valid_length.O)
                    is_valid = is_valid_cmp.O

                wire(inner_counters.valid & is_valid, cls.valid)
                if has_last:
                    wire(is_last & inner_counters.last, cls.last)
                if has_reset:
                    wire(cls.RESET, outer_counter.RESET)
                    wire(cls.RESET, inner_counters.RESET)
                    if has_cur_valid:
                        wire(cls.RESET, cur_valid_counter.RESET)
                if has_ce:
                    wire(bit(cls.CE) & inner_counters.last, outer_counter.CE)
                    wire(cls.CE, inner_counters.CE)
                    if has_cur_valid:
                        wire(bit(cls.CE) & inner_counters.valid & is_valid, cur_valid_counter.CE)
                else:
                    wire(inner_counters.last, outer_counter.CE)
                    if has_cur_valid:
                        wire(inner_counters.valid & is_valid, cur_valid_counter.CE)
            elif is_nested(t):
                inner_counters = DefineNestedCounters(t.t, has_last, has_cur_valid, has_ce, has_reset)()

                wire(inner_counters.valid, cls.valid)
                if has_last:
                    wire(inner_counters.last, cls.last)
                if has_reset:
                    wire(cls.RESET, inner_counters.RESET)
                if has_ce:
                    wire(cls.CE, inner_counters.CE)
                if has_cur_valid:
                    wire(inner_counters.cur_valid, cls.cur_valid)
            else:
                # only 1 element, so always last and valid element
                valid_and_last = DefineCoreirConst(1, 1)()
                if has_last:
                    wire(valid_and_last.O[0], cls.last)
                if has_cur_valid:
                    cur_valid = DefineCoreirConst(1, 0)()
                    wire(cur_valid.O, cls.cur_valid)
                if has_ce:
                    wire(cls.valid, cls.CE)
                else:
                    wire(valid_and_last.O[0], cls.valid)
                if has_reset:
                    reset_term = TermAnyType(Bit)
                    wire(reset_term.I, cls.RESET)

    return _NestedCounters

def NestedCounters(t: ST_Type, has_cur_valid=False, has_ce=False, has_reset=False) -> Circuit:
    return DefineRAMAnyType(t, has_cur_valid=has_cur_valid, has_ce=has_ce, has_reset=has_reset)()
