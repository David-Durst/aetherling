from magma import *
from magma.circuit import DefineCircuitKind
from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import CircuitInstanceFromGeneratorWrapper
from ..helpers.nameCleanup import cleanName
from magma.t import Kind
from mantle.coreir.type_helpers import Term
from .hydrate import DefineDehydrate, Dehydrate, DefineHydrate, Hydrate
from .map_fully_parallel_sequential import MapParallel

def DefineSerializer(cirb: CoreIRBackend, T: Kind, N: int, has_count=False, has_ce=False, has_reset=False) -> DefineCircuitKind:
    """
    Serializer converts an array to a stream and emit the stream over multiple clocks
    Aetherling Type: ({1, Array(N, T)} -> {N, T}, N)
    This returns a circuit definition.

    :param cirb: The CoreIR backend currently be used
    :param T: The type of the elements in the array to serialize
    :param N: The length of the array to serialize
    :param has_count: Whether to have a port on serializer of length equal to the
    width of T in bits that emits the current element being emitted by the serializer
    :param has_ce: Whether to have a CE port on serializer
    :param has_reset: Whether to have a reset port on serializer
    :return: A module with the following ports:
        I : In(Array(N, T))
        out : Out(T)
        ready : Out(Bit)
        and others depending on has_ arguments
    """
    if N <= 0:
        raise ValueError("Serialize must work on an array of at least 1. N <= 0")

    class _Serializer(Circuit):
        name = "serialize_t{}_n{}".format(cleanName(str(T)), str(N))

        if has_count:
            cirType = cirb.get_type(T, True)
            count_interface = ["count", Out(Array(T.size, Bit))]
        else:
            count_interface = []

        IO = ["I", In(Array(N, T)), "out", Out(T), "ready", Out(Bit)] + \
             ClockInterface(has_ce, has_reset) + count_interface

        @classmethod
        def definition(serializer):
            # CoreIR serializer works on arrays of arrays of bits, where each T is an array of bits
            # so need to hydrate and dehydrate
            dehydrate = MapParallel(cirb, N, DefineDehydrate(cirb, T))
            hydrate = Hydrate(cirb, T)
            coreirSerializer = CircuitInstanceFromGeneratorWrapper(cirb, "commonlib", "serializer",
                                                                   "dehydrated_" + serializer.name,
                                                                   ["mantle", "coreir", "global"],
                                                                   {"width": hydrate.size, "rate": N})

            wire(serializer.I, dehydrate.I)
            wire(dehydrate.out, coreirSerializer.I)
            wire(coreirSerializer.out, hydrate.I)
            wire(hydrate.out, serializer.out)
            wire(coreirSerializer.ready, serializer.ready)

            if has_ce:
                wire(serializer.CE, coreirSerializer.en)
            else:
                wire(VCC, coreirSerializer.en)
            if has_reset:
                wire(serializer.RESET, coreirSerializer.reset)
            else:
                wire(GND, coreirSerializer.reset)
            if has_count:
                wire(coreirSerializer.count, serializer.count)
            else:
                countTerm = Term(cirb, hydrate.size)
                wire(coreirSerializer.count, countTerm.I)

    return _Serializer

def Serializer(cirb: CoreIRBackend, T: Kind, N: int, has_count=False, has_ce=False, has_reset=False) -> Circuit:
    """
    Serializer converts an array to a stream and emit the stream over multiple clocks
    Aetherling Type: ({1, Array(N, T)} -> {N, T}, N)
    This returns a circuit instance, an instance of a circuit definition

    :param cirb: The CoreIR backend currently be used
    :param T: The type of the elements in the array to serialize
    :param N: The length of the array to serialize
    :param has_count: Whether to have a port on serializer of length equal to the
    width of T in bits that emits the current element being emitted by the serializer
    :param has_ce: Whether to have a CE port on serializer
    :param has_reset: Whether to have a reset port on serializer
    :return: A module with the following ports:
        I : In(Array(N, T))
        out : Out(T)
        valid : Out(Bit)
        and others depending on has_ arguments
    """
    return DefineSerializer(cirb, T, N, has_count, has_ce, has_reset)()

def DefineDeserializer(cirb: CoreIRBackend, T: Kind, N: int, has_ce=False, has_reset=False) -> DefineCircuitKind:
    """
    Deserializer converts a stream to an array over multiple clocks
    Aetherling Type: ({N, T} -> {1, Array(N, T)}, N)
    This returns a circuit definition.

    :param cirb: The CoreIR backend currently be used
    :param T: The type of the elements in the array to deserialize
    :param N: The length of the array to deserializer
    :param has_ce: Whether to have a CE port on deserializer
    :param has_reset: Whether to have a reset port on deserializer
    :return: A module with the following ports:
        I : In(T)
        out : Out(Array(N, T))
        valid : Out(Bit)
        and others depending on has_ arguments
    """
    if N <= 0:
        raise ValueError("Serialize must work on an array of at least 1. N <= 0")

    class _Deserializer(Circuit):
        name = "serialize_t{}_n{}".format(cleanName(str(T)), str(N))

        IO = ["I", In(T), "out", Out(Array(N, T)), "valid", Out(Bit)] + \
             ClockInterface(has_ce, has_reset)

        @classmethod
        def definition(serializer):
            # CoreIR serializer works on arrays of arrays of bits, where each T is an array of bits
            # so need to hydrate and dehydrate
            hydrate = MapParallel(cirb, N, DefineHydrate(cirb, T))
            dehydrate = Dehydrate(cirb, T)
            coreirDeserializer = CircuitInstanceFromGeneratorWrapper(cirb, "commonlib", "deserializer",
                                                                     "dehydrated_" + serializer.name,
                                                                     ["mantle", "coreir", "global"],
                                                                     {"width": dehydrate.size, "rate": N})

            wire(serializer.I, hydrate.I)
            wire(hydrate.out, coreirDeserializer.I)
            wire(coreirDeserializer.out, dehydrate.I)
            wire(dehydrate.out, serializer.out)
            wire(coreirDeserializer.valid, serializer.valid)

            if has_ce:
                wire(serializer.CE, coreirDeserializer.en)
            else:
                wire(VCC, coreirDeserializer.en)
            if has_reset:
                wire(serializer.RESET, coreirDeserializer.reset)
            else:
                wire(GND, coreirDeserializer.reset)

    return _Deserializer

def Deserializer(cirb: CoreIRBackend, T: Kind, N: int, has_ce=False, has_reset=False) -> Circuit:
    """
    Deserializer converts a stream to an array over multiple clocks
    Aetherling Type: ({N, T} -> {1, Array(N, T)}, N)
    This returns a circuit instance, an instance of a circuit definition

    :param cirb: The CoreIR backend currently be used
    :param T: The type of the elements in the array to deserialize
    :param N: The length of the array to deserializer
    :param has_ce: Whether to have a CE port on deserializer
    :param has_reset: Whether to have a reset port on deserializer
    :return: A module with the following ports:
        I : In(T)
        out : Out(Array(N, T))
        valid : Out(Bit)
        and others depending on has_ arguments
    """
    return DefineDeserializer(cirb, T, N, has_ce, has_reset)()