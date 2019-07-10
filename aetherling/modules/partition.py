from magma import *
from .hydrate import Hydrate, Dehydrate
from .map_fully_parallel_sequential import MapParallel
from mantle.coreir.MUX import CommonlibMuxN
from mantle.common.countermod import SizedCounterModM
from ..helpers.nameCleanup import cleanName

def DefinePartition(arrayType: ArrayKind, subsetSize: int, has_ce=False):
    """
    Split an array of elements in arrayLength/subsetSize elements of size subsetSize each.
    Aetherling Type: {1, T[k]} -> {k/s, T[s]}
    (k = arrayLength, s = subsetSize)
    Note: must partition into evenly sized subsets, so k % s == 0

    :param arrayType: The type T[k] that will be split into subsets
    :param numSubsets: How big each subset should be
    :return: A module with the following ports:
    I : In(T[k])
    CE : In(Enable)
    O : O(T[s])
    """
    assert arrayType.N >= 1, "Must partition array of at least length 1"
    assert arrayType.N % subsetSize == 0, "At this time, must partition into evenly sized arrays"

    class Partition(Circuit):
        # https://stackoverflow.com/questions/12985456/replace-all-non-alphanumeric-characters-in-a-string
        name = "Partition" + "".join([ c if c.isalnum() else "_" for c in \
                                       str(cleanName(str(arrayType)))]) + "_" + str(subsetSize)
        elementType = arrayType.T
        IO = ['I', In(arrayType), 'O', Out(Array[subsetSize, elementType])] + \
            ClockInterface(has_ce=has_ce)
        @classmethod
        def definition(partition):
            dehydrate = MapParallel(arrayType.N, Dehydrate(partition.elementType))
            # each mux emits 1 element of the subset that is emitted every clock
            # each mux needs to handle k/s inputs, so it can output one element every clock
            muxes = MapParallel(subsetSize, CommonlibMuxN(int(arrayType.N / subsetSize),
                                              len(dehydrate.out[0])))
            hydrate = MapParallel(subsetSize, Hydrate(partition.elementType))
            counter = SizedCounterModM(int(arrayType.N/subsetSize), has_ce=has_ce)

            wire(partition.I, dehydrate.I)
            for i in range(subsetSize):
                # to the first mux wire 0, subsetSize, 2*subsetSize,...
                # so that each clock it emits the first element of the next subset
                # repeat for each mux so ith mux outputs ith element of subset each clock
                wire(dehydrate.out[i::subsetSize], muxes.I[i].data)
                wire(counter.O, muxes.I[i].sel)
            wire(muxes.out, hydrate.I)
            wire(hydrate.out, partition.O)
            if has_ce:
                wire(partition.CE, counter.CE)
    return Partition

def Partition(arrayType: Type, subsetSize: int, has_ce=False):
    return DefinePartition(arrayType, subsetSize, has_ce)()

def DefinePartitionParallel(T: Kind, outer_len: int, inner_len: int, has_ready_valid=False):
    """
    Convert an SSeq (no*ni) T to and SSeq ni (SSeq no T)

    The time_per_element clock cycles in a period is not relevant for this operator as it is combinational.

    Note that the T passed to this operator just the Magma type each clock cycle.
    You can get T by calling magma_repr on a space-time type T'.

    I : In(Array[outer_len * inner_len, T])
    O : Out(Array[outer_len, Array[inner_len, T]])
    if has_ready_valid:
    ready_up : Out(Bit)
    valid_up : In(Bit)
    ready_down : In(Bit)
    valid_down : Out(Bit)
    """
    class PartitionParallel(Circuit):
        name = "PartitionParallel_T{}_outer{}_inner{}_hasReadyValid{}".format(cleanName(str(T)), str(outer_len),
                                                                              str(inner_len), str(has_ready_valid))
        IO = ['I', In(Array[outer_len * inner_len, T]), 'O', Out(Array[outer_len, Array[inner_len, T]])]
        @classmethod
        def definition(partitionParallel):
            for i in range(outer_len):
                for j in range(inner_len):
                    wire(partitionParallel.I[i * inner_len + j], partitionParallel.O[i][j])
            if has_ready_valid:
                wire(partitionParallel.ready_up, partitionParallel.ready_down)
                wire(partitionParallel.valid_up, partitionParallel.valid_down)
    return PartitionParallel

def PartitionParallel(T: Kind, outer_len: int, inner_len: int, has_ready_valid=False):
    return DefinePartitionParallel(T, outer_len, inner_len, has_ready_valid)

def DefineUnpartitionParallel(T: Kind, outer_len: int, inner_len: int, has_ready_valid=False):
    """
    Convert an SSeq no (Seq ni T) to and SSeq (no*ni) T

    The time_per_element clock cycles in a period is not relevant for this operator as it is combinational.

    Note that the T passed to this operator just the Magma type each clock cycle.
    You can get T by calling magma_repr on a space-time type T'.

    I : In(Array[outer_len, Array[inner_len, T]])
    O : Out(Array[outer_len * inner_len, T])
    if has_ready_valid:
    ready_up : Out(Bit)
    valid_up : In(Bit)
    ready_down : In(Bit)
    valid_down : Out(Bit)
    """
    class PartitionParallel(Circuit):
        name = "UnpartitionParallel_T{}_outer{}_inner{}_hasReadyValid{}".format(cleanName(str(T)), str(outer_len),
                                                                              str(inner_len), str(has_ready_valid))
        IO = ['I', In(Array[outer_len, Array[inner_len, T]]), 'O', Out(Array[outer_len * inner_len, T])]
        @classmethod
        def definition(partitionParallel):
            for i in range(outer_len):
                for j in range(inner_len):
                    wire(partitionParallel.O[i * inner_len + j], partitionParallel.I[i][j])
            if has_ready_valid:
                wire(partitionParallel.ready_up, partitionParallel.ready_down)
                wire(partitionParallel.valid_up, partitionParallel.valid_down)
    return PartitionParallel

def PartitionParallel(T: Kind, outer_len: int, inner_len: int, has_ready_valid=False):
    return DefinePartitionParallel(T, outer_len, inner_len, has_ready_valid)
