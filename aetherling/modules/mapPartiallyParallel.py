from .mapFullyParallelSequential import MapParallel
from magma import *
from magma.backend.coreir_ import CoreIRBackend
from .partition import Partition
from ..helpers.nameCleanup import cleanName

@cache_definition
def DefineMapPartiallyParallel(cirb: CoreIRBackend, numInputs: int, parallelism: float,
                         op: Circuit, has_ce=False) -> Circuit:
    assert numInputs/parallelism % 1 == 0, "Parallelism must divide into numInputs with no " \
                                           "remainder"
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
      I : In(Array(numInputs, T)
      O : Out(Array(numInputs/parallelism, S))
    """
    class _MapPartiallyParallel(Circuit):
        name = "Map_n{}_p{}_op{}".format(str(numInputs), str(parallelism), cleanName(str(type(op))))
        # extend each input to length of numInputs, each output to parallelism length
        inputs = [nameOrPort if type(nameOrPort) == str else Array(numInputs, type(nameOrPort)) for nameOrPort in
                  op.inputargs()]
        outputs = [nameOrPort if type(nameOrPort) == str else \
                       Array(parallelism, type(nameOrPort)) for nameOrPort in
                   op.outputargs()]
        IO = inputs + outputs + ClockInterface(has_ce=has_ce) + ['test', Array(3, Array(8, Out(Bit)))]

        @classmethod
        def definition(mapPartiallyParallel):
            inputNames = [name for name in mapPartiallyParallel.inputs[::2]]
            outputNames = [name for name in mapPartiallyParallel.outputs[::2]]
            ops = MapParallel(cirb, parallelism, op)
            # wire each input (which has been partitioned into subsets) into each operation
            for inputName in inputNames:
                inputPartition = Partition(cirb, type(getattr(mapPartiallyParallel, inputName)),
                                           parallelism, has_ce=has_ce)
                wire(getattr(mapPartiallyParallel, inputName), inputPartition.I)
                wire(inputPartition.O, getattr(ops, inputName))
                if inputName == 'I0':
                    wire(inputPartition.O[0], mapPartiallyParallel.test)
            # wire each each op (for one subset) to an output
            for outputName in outputNames:
                wire(getattr(ops, outputName), getattr(mapPartiallyParallel, outputName))
    return _MapPartiallyParallel


def MapPartiallyParallel(cirb: CoreIRBackend, numInputs: int, parallelism: int,
                         op: Circuit, has_ce=False):
    return DefineMapPartiallyParallel(cirb, numInputs, parallelism, op, has_ce)()