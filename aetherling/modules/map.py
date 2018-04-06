from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import CircuitInstanceFromGeneratorWrapper, GetCoreIRModule
from magma import *
from .partition import Partition

def MapParallel(cirb: CoreIRBackend, numInputs: int, op: Circuit) -> Circuit:
    """
    Map an operation over numInputs inputs in one clock cycle
    Aetherling Type: {1, T[numInputs]} -> {1, S[numInputs]}

    :param cirb: The CoreIR backend currently be used
    :param numInputs: The number of input elements
    :param op: The operator (the magma circuit) to map over the elements. It should have type T -> S
    :return: A module with all the ports of op, except that they will be converted to arrays
     of original type, with each array of length numInputs. For example, if op has
     one input port I and one output O, the ports will be:
    I : In(Array(numInputs, T))
    O : Out(Array(numInputs, S))
    """
    if op.defn.instances.__contains__(op):
        op.defn.instances.remove(op)
    moduleToReturn = CircuitInstanceFromGeneratorWrapper(cirb, "aetherlinglib", "mapParallel",
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         {"numInputs": numInputs,
                                                 "operator": GetCoreIRModule(cirb, op)})
    return moduleToReturn


def MapSequential(cirb: CoreIRBackend, numInputs: int, op: Circuit, opContainer: Circuit) -> Circuit:
    """
    Map an operation over numInputs inputs over numInputs cycles.
    Note: the entire inputs must be delivered on the first cycle.
    There is no point in implementing the streaming version of this operation
    as that is just the module op.
    Aetherling Type: {1, T[numInputs]} -> {1, S[numInputs]}

    :param cirb: The CoreIR backend currently be used
    :param numInputs: The number of input elements
    :param op: The operator (the coreir module) to map over the elements. It should
    s.t. Op : T -> S
    :return: A module with all the ports of op, except that they will be converted to arrays
     of original type, with each array of length numInputs. For example, if op has
     one input port I and one output O, the ports will be:
     I : In(Array(numInputs, T)
     O : Out(Array(numInputs, S))
    """
    if opContainer.instances.__contains__(op):
        opContainer.instances.remove(op)
    moduleToReturn = CircuitInstanceFromGeneratorWrapper(cirb, "aetherlinglib", "mapSequential",
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         {"numInputs": numInputs,
                                                 "operator": GetCoreIRModule(cirb, op)})
    return moduleToReturn

def DefineMapPartiallyParallel(cirb: CoreIRBackend, numInputs: int, parallelism: int,
                         op: Circuit, has_ce=False) -> Circuit:
    """
    Map an operation over numInputs inputs in numInputs/parallelism clock cycles
    Aetherling Type: {numInputs/parallelism, T[numInputs]} -> {numInputs/parallelism, S[numInputs]}

    :param cirb: The CoreIR backend currently be used
    :param numInputs: The number of input elements
    :param op: The operator (the magma circuit) to map over the elements. It should have type T -> S
    :return: A module with all the ports of op, except that they will be converted to arrays
     of original type. Input ports will be of length numInputs, output ports will be of type
      parallelism. For example, if op has one input port I and one output O, the ports
      will be:
      I : In(Array(numInputs, T)
      O : Out(Array(parallelism, S))
    """
    class MapPartiallyParallel(Circuit):
        name = "Map" + str(numInputs) + "_" + str(parallelism)
        # extend each input to length of numInputs, each output to parallelism length
        inputs = [[name, Array(numInputs, port)] for [name, port] in op.inputargs()]
        outputs = [[name, Array(parallelism, port)] for [name, port] in op.outputsargs()]
        IO = flatten(inputs + outputs)

        @classmethod
        def definition(mapPartiallyParallel):
            inputNames = [name for [name, _] in mapPartiallyParallel.inputs]
            outputNames = [name for [name, _] in mapPartiallyParallel.outputs]
            ops = MapParallel(cirb, parallelism, op)
            # wire each input (which has been partitioned into subsets) into each operation
            for inputName in inputNames:
                inputPartition = MapParallel(cirb, parallelism,
                                        Partition(cirb, getattr(mapPartiallyParallel, inputName).T,
                                                  parallelism, has_ce=has_ce))
                wire(getattr(mapPartiallyParallel, inputName), inputPartition.I)
                wire(inputPartition.O, getattr(ops, inputName))
            # wire each each op (for one subset) to an output
            for outputName in outputNames:
                wire(getattr(ops, outputName), getattr(mapPartiallyParallel, outputName))
    return MapPartiallyParallel

def MapPartiallyParallelcirb(cirb: CoreIRBackend, numInputs: int, parallelism: int,
                         op: Circuit, has_ce=False):
    return DefineMapPartiallyParallel(cirb, numInputs, parallelism, op, has_ce)()
