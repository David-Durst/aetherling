from magma.backend.coreir_ import CoreIRBackend
from magma import *
from .hydrate import Hydrate, Dehydrate
from .mapFullyParallelSequential import MapParallel
from mantle.coreir.MUX import CommonlibMuxN
from mantle import SizedCounterModM

@cache_definition
def DefinePartition(cirb: CoreIRBackend, arrayType: Type, subsetSize: int,
              has_ce=False):
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
        name = "Partition" + str(arrayType) + "_" + str(subsetSize)
        elementType = arrayType.T
        IO = ['I', In(arrayType), 'O', Out(Array(subsetSize, elementType))] + \
            ClockInterface(has_ce=has_ce)
        @classmethod
        def definition(partition):
            dehydrate = MapParallel(cirb, arrayType.N, Dehydrate(cirb, partition.elementType))
            # each mux emits 1 element of the subset that is emitted every clock
            # each mux needs to handle k/s inputs, so it can output one element every clock
            muxes = MapParallel(cirb, subsetSize,
                                CommonlibMuxN(cirb, arrayType.N/subsetSize, len(dehydrate.out[0])))
            hydrate = MapParallel(cirb, subsetSize, Hydrate(cirb, partition.elementType))
            counter = SizedCounterModM(arrayType.N/subsetSize, has_ce=has_ce)

            wire(partition.I, dehydrate.I)
            for i in range(subsetSize):
                # to the first mux wire 0, subsetSize, 2*subsetSize,...
                # so that each clock it emits the first element of the next subset
                # repeat for each mux so ith mux outputs ith element of subset each clock
                wire(dehydrate.O[i::subsetSize], muxes.I[0].data)
                wire(counter.O, muxes.I[0].sel)
            wire(muxes.out, hydrate.I)
            wire(hydrate.I, partition.O)
            if has_ce:
                wire(partition.CE, counter.CE)
    return Partition

def Partition(cirb: CoreIRBackend, arrayType: Type, subsetSize: int, has_ce=False):
    return DefinePartition(cirb, arrayType, subsetSize, has_ce)()