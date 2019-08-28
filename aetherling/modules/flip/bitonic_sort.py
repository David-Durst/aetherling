from mantle.common.countermod import SizedCounterModM
from mantle.common.operator import *
from aetherling.modules.term_any_type import TermAnyType
from aetherling.modules.ram_any_type import DefineRAMAnyType
from aetherling.modules.register_any_type import DefineRegisterAnyType
from aetherling.modules.mux_any_type import DefineMuxAnyType
from magma import *
from magma.bitutils import int2seq
from magma.circuit import DefineCircuitKind
from aetherling.modules.hydrate import Dehydrate, Hydrate
from aetherling.modules.map_fully_parallel_sequential import MapParallel
from aetherling.helpers.nameCleanup import cleanName
from aetherling.helpers.magma_helpers import ready_valid_interface
from mantle.coreir.memory import getRAMAddrWidth
from mantle.common.countermod import SizedCounterModM, DefineCounterModM
from mantle.coreir.compare import DefineCoreirUlt
from mantle import Decode
from typing import Callable


@cache_definition
def DefineSort2Elements(T: Kind, cmp_component: Callable[[DefineCircuitKind], Kind] = id):
    """
    Given two elements each of type T, emit the smaller one on O0 and the larger one on O1.
    cmp_component is a function that selects the components of each T to compare.
    Smaller and larger is determined by < on the flattened bits of each elements.

    Note that the T passed to this operator just the Magma type each clock cycle.
    You can get T by calling magma_repr on a space-time type T'.

    I0 : T
    I1 : T
    O0 : T
    O1 : T
    """
    class _Sort2Elements(Circuit):
        name = "Sort2Elements_T{}".format(cleanName(str(T)))
        IO = ['I0', In(T), 'I1', In(T), 'O0', Out(T), 'O1', Out(T)] + ClockInterface()
        @classmethod
        def definition(Sort2Elements):
            sel = DefineMuxAnyType(t,2)

            cmp0 = cmp_component(Sort2Elements.I0)
            cmp1 = cmp_component(Sort2Elements.I1)

            cmp0_bits = Dehydrate(type(cmp0))
            cmp1_bits = Dehydrate(type(cmp1))

            wire(cmp0_bits.I, cmp0)
            wire(cmp1_bits.I, cmp1)

            lt = DefineCoreirUlt(cmp0_bits.O.N)

            wire(lt.I0, cmp0_bits.O)
            wire(lt.I1, cmp1_bits.O)

            wire(Sort2Elements.I0, sel.data[0])
            wire(Sort2Elements.I1, sel.data[1])
            wire(lt.O, sel.sel)

            wire(sel.out, Sort2Elements.out)

    return _Sort2Elements
