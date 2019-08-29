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
from math import log2

@cache_definition
def DefineBitonicSortPow2(T: Kind, n: int, sort2LtDef: DefineCircuitKind):
    """

    :param T: The type of each element in the sort
    :param n: The number of elements to sort
    :param sort2LtDef: A circuit definition that accepts two inputs of type T and emits the lesser on out O0
    and greater on out O1.
    :return: A circuit with ports
    I : Array[n, T]
    O : Array[n, T]

    """
    class _BitonicSortPow2(Circuit):
        name = "BitonicMergePow2_t{}_n{}_sort2LtDef{}".format(cleanName(str(T)), str(n), cleanName((str(sort2LtDef))))
        IO = ['I', In(Array[n, T]), 'O', Out(Array[n, T])]
        @classmethod
        def definition(BitonicSortPow2):
            # input ports and all other intermediate merge ports
            # starting with just input ports
            ports = [[BitonicSortPow2.I[i] for i in range(n)]]
            # Sort ranges starting with 2^1 until 2^m == n
            for i in range(1,int(log2(n))):
                elements_per_merge = pow(2, i)
                cur_prior_stage_port = 0
                cur_stage_ports = []
                for j in range(0,i):
                    merger = DefineBitonicMergePow2(T, elements_per_merge, sort2LtDef, j)
                    for k in range(0,elements_per_merge):
                        wire(ports[i][cur_prior_stage_port], merger.I[k])
                        cur_prior_stage_port += 1
                        cur_stage_ports += [merger.O[k]]
            last_ports = ports[-1]
            for i in range(len(last_ports)):
                wire(last_ports[i], BitonicSortPow2.O[i])
    return _BitonicSortPow2

@cache_definition
def DefineBitonicMergePow2(T: Kind, n: int, sort2LtDef: DefineCircuitKind, ith_merge: int = 0):
    """

    :param T: The type of each element in the sort
    :param n: The number of elements to sort
    :param sort2LtDef: A circuit definition that accepts two inputs of type T and emits the lesser on out O0
    and greater on out O1.
    :param ith_merge: The index of this merge. If multiple merge's aligned vertically, alternate directions
    :return: A circuit with ports
    I : Array[n, T]
    O : Array[n, T]

    """
    class _BitonicMergePow2(Circuit):
        name = "BitonicMergePow2_t{}_n{}_sort2LtDef{}_ithMerge".format(cleanName(str(T)), str(n),
                                                                       cleanName((str(sort2LtDef))), str(ith_merge))
        IO = ['I', In(Array[n, T]), 'O', Out(Array[n, T])]
        @classmethod
        def definition(BitonicMergePow2):
            # first sort the inputs once
            first_sorts = []
            for i in range(n // 2):
                pair_sort = sort2LtDef()
                if ith_merge % 2 == 0:
                    wire(BitonicMergePow2.I[i*2], pair_sort.I0)
                    wire(BitonicMergePow2.I[i*2+1], pair_sort.I1)
                else:
                    wire(BitonicMergePow2.I[i*2], pair_sort.I1)
                    wire(BitonicMergePow2.I[i*2+1], pair_sort.I0)
                first_sorts += [pair_sort]

            # next merge each of the halfs
            mergers = [DefineBitonicMergePow2(T, n // 2, sort2LtDef, ith_merge)(),
                       DefineBitonicMergePow2(T, n // 2, sort2LtDef, ith_merge)()]
            for i in range(n // 4):
               wire(pair_sort[i*2].O0, mergers[0].I[i*2])
               wire(pair_sort[i*2].O1, mergers[0].I[i*2+1])
               wire(mergers[0].O[i*2], BitonicMergePow2.O[i*2])
               wire(mergers[0].O[i*2+1], BitonicMergePow2.O[i*2+1])

               wire(pair_sort[(n // 2) + i*2].O0, mergers[1].I[i*2])
               wire(pair_sort[(n // 2) + i*2].O1, mergers[1].I[i*2+1])
               wire(mergers[1].O[i*2], BitonicMergePow2.O[(n // 2) + i*2])
               wire(mergers[1].O[i*2+1], BitonicMergePow2.O[(n // 2) + i*2+1])

    return _BitonicMergePow2


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
            sel = DefineMuxAnyType(Array[2, T],2)()

            cmp0 = cmp_component(Sort2Elements.I0)
            cmp1 = cmp_component(Sort2Elements.I1)

            cmp0_bits = Dehydrate(type(cmp0))
            cmp1_bits = Dehydrate(type(cmp1))

            wire(cmp0_bits.I, cmp0)
            wire(cmp1_bits.I, cmp1)

            lt = DefineCoreirUlt(cmp0_bits.out.N)()

            wire(lt.I0, cmp0_bits.out)
            wire(lt.I1, cmp1_bits.out)

            # lt will emit 1 if I0 is less than
            wire(Sort2Elements.I0, sel.data[1][0])
            wire(Sort2Elements.I1, sel.data[1][1])
            wire(Sort2Elements.I0, sel.data[0][1])
            wire(Sort2Elements.I1, sel.data[0][0])
            wire(lt.O, sel.sel[0])

            wire(sel.out[0], Sort2Elements.O0)
            wire(sel.out[1], Sort2Elements.O1)

    return _Sort2Elements
