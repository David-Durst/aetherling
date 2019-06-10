from .map_fully_parallel_sequential import MapParallel
from magma import *
from .partition import Partition
from ..helpers.nameCleanup import cleanName
from aetherling.helpers.magma_helpers import getOutputPorts, getInputPorts

@cache_definition
def DefineMapPartiallyParallel(numInputs: int, parallelism: float,
                         op: Circuit, has_ce=False) -> Circuit:
    """
    Map an operation over numInputs inputs in numInputs/parallelism clock cycles
    Aetherling Type: {1, T[numInputs]} -> {numInputs/parallelism, S[parallelism]}

    :param cirb: The CoreIR backend currently be used
    :param numInputs: The number of input elements
    :param op: The operator (the magma circuit) to map over the elements. It should have type T -> S
    :return: A module with all the ports of op, except that they will be converted to arrays
     of original type. Input ports will be of length numInputs, output ports will be of length
      numInputs/parallelism. For example, if op has one input port I and one output O, the ports
      will be:
      I : In(Array[numInputs, T]
      O : Out(Array[numInputs/parallelism, S)])
    """
    
    assert numInputs/parallelism % 1 == 0, "Parallelism must divide into numInputs with no " \
                                           "remainder"

    class _MapPartiallyParallel(Circuit):
        name = "Map_n{}_p{}_op{}".format(str(numInputs), str(parallelism), cleanName(str(op)))
        # extend each input to length of numInputs, each output to parallelism length
        # getting output ports for inputs and vice versa since
        inputs = [nameOrPort if type(nameOrPort) == str else Array[numInputs, nameOrPort]
                  for nameAndPort in getInputPorts(op.IO) for nameOrPort in nameAndPort]
        outputs = [nameOrPort if type(nameOrPort) == str else Array[parallelism, nameOrPort]
                   for nameAndPort in getOutputPorts(op.IO) for nameOrPort in nameAndPort]
        IO = inputs + outputs + ClockInterface(has_ce=has_ce)

        @classmethod
        def definition(mapPartiallyParallel):
            inputNames = [name for name in mapPartiallyParallel.inputs[::2]]
            outputNames = [name for name in mapPartiallyParallel.outputs[::2]]
            ops = MapParallel(parallelism, op)
            # wire each input (which has been partitioned into subsets) into each operation
            for inputName in inputNames:
                inputPartition = Partition(type(getattr(mapPartiallyParallel, inputName)),
                                           parallelism, has_ce=has_ce)
                wire(getattr(mapPartiallyParallel, inputName), inputPartition.I)
                wire(inputPartition.O, getattr(ops, inputName))
                if has_ce:
                    wire(mapPartiallyParallel.CE, inputPartition.CE)
            # wire each each op (for one subset) to an output
            for outputName in outputNames:
                wire(getattr(ops, outputName), getattr(mapPartiallyParallel, outputName))
    return _MapPartiallyParallel


def MapPartiallyParallel(numInputs: int, parallelism: int, op: Circuit, has_ce=False):
    return DefineMapPartiallyParallel(numInputs, parallelism, op, has_ce)()