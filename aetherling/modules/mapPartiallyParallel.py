from aetherling.modules.mapFullyParallelSequential import MapParallel
from magma import Circuit, Array, flatten, wire
from magma.backend.coreir_ import CoreIRBackend
from .partition import Partition


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
                                    aePartition.Partition(cirb, getattr(mapPartiallyParallel, inputName).T,
                                                  parallelism, has_ce=has_ce))
                wire(getattr(mapPartiallyParallel, inputName), inputPartition.I)
                wire(inputPartition.O, getattr(ops, inputName))
            # wire each each op (for one subset) to an output
            for outputName in outputNames:
                wire(getattr(ops, outputName), getattr(mapPartiallyParallel, outputName))
    return MapPartiallyParallel


def MapPartiallyParallel(cirb: CoreIRBackend, numInputs: int, parallelism: int,
                         op: Circuit, has_ce=False):
    return DefineMapPartiallyParallel(cirb, numInputs, parallelism, op, has_ce)()