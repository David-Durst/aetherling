from aetherling.space_time.space_time_types import *
from aetherling.space_time.type_helpers import ST_Tombstone, is_nested
from aetherling.modules.term_any_type import TermAnyType
from mantle.coreir import DefineCoreirConst, DefineCoreirUlt
from mantle.common.countermod import Decode
from aetherling.modules.ram_any_type import *
from aetherling.helpers.nameCleanup import cleanName
from mantle.common.countermod import SizedCounterModM
from magma import *
from magma.circuit import DefineCircuitKind, Circuit
from typing import List

__all__ = ['DefineNestedCounters', 'NestedCounters']

@cache_definition
def DefineNestedCounters(t: ST_Type, has_ce: bool = False, has_reset: bool = False) -> DefineCircuitKind:
    """
    Generate a set of nested counters that emit valid according to the specified type t.
    Last indicates whether this is the last clock (not period) of the type

    valid: Out(Bit)
    last: Out(Bit)
    if has_ce:
    CE : In(Bit)
    if has_reset:
    RESET : In(Bit)
    """

    class _NestedCounters(Circuit):
        name = 'NestedCounters_{}_hasCE{}_hasReset{}'.format(cleanName(str(t)), str(has_ce), str(has_reset))
        IO = ['valid', Out(Bit), 'last', Out(Bit)] + ClockInterface(has_ce=has_ce, has_reset=has_reset)
        @classmethod
        def definition(cls):
            if type(t) == ST_TSeq:
                outer_counter = SizedCounterModM(t.n + t.i, has_ce= True, has_reset=has_reset)
                inner_counters = DefineNestedCounters(t.t, has_ce, has_reset)()
                is_last = Decode(t.n + t.i - 1, outer_counter.O.N)(outer_counter.O)
                valid_length = DefineCoreirConst(outer_counter.O.N, t.n)()
                is_valid = DefineCoreirUlt(outer_counter.O.N)()

                wire(is_valid.I0, outer_counter.O)
                wire(is_valid.I1, valid_length.O)

                wire(inner_counters.valid & is_valid.O, cls.valid)
                wire(is_last & inner_counters.last, cls.last)
                if has_reset:
                    wire(cls.RESET, outer_counter.RESET)
                    wire(cls.RESET, inner_counters.RESET)
                if has_ce:
                    wire(cls.CE, outer_counter.CE & inner_counters.last)
                    wire(cls.CE, inner_counters.CE)
                else:
                    wire(inner_counters.last, outer_counter.CE)
            elif is_nested(t):
                inner_counters = DefineNestedCounters(t.t, has_ce, has_reset)()

                wire(inner_counters.valid, cls.valid)
                wire(inner_counters.last, cls.last)
                if has_reset:
                    wire(cls.RESET, inner_counters.RESET)
                if has_ce:
                    wire(cls.CE, inner_counters.CE)
            else:
                # only 1 element, so always last and valid element
                valid_and_last = DefineCoreirConst(1, 1)()
                wire(valid_and_last.O[0], cls.valid)
                wire(valid_and_last.O[0], cls.last)

    return _NestedCounters

def NestedCounters(t: ST_Type, has_ce=False, has_reset=False) -> Circuit:
    return DefineRAMAnyType(t, has_ce=has_ce, has_reset=has_reset)()
