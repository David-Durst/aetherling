from magma.backend.coreir_ import CoreIRBackend
from magma import *
from .hydrate import Hydrate, Dehydrate
from .map import MapParallel
from mantle import Mux

def Partition(cirb: CoreIRBackend, arrayType: Type, subsetSize: int):
    """
    Split an array of elements in arrayLength/subsetSize elements of size subsetSize each.
    Aetherling Type: {1, T[k]} -> {k/p, T[s]}
    (k = arrayLength, s = subsetSize)

    :param arrayType: The type T[k] that will be split into subsets
    :param numSubsets: How big each subset should be
    :return: A module with the following ports:
    I : In(T[k])
    O : O(T[s])
    """
    class Partition(Circuit):
        name = "Partition" + str(arrayType) + "_" + str(subsetSize)
        elementType = arrayType.T
        IO = ['I', In(arrayType), 'O', Out(Array(subsetSize, elementType))]
        @classmethod
        def definition(partition):
            dehydrate = Dehydrate(cirb, partition.elementType)
            dehydrateAllInputs =