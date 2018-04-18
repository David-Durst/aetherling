from magma import *
from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import CircuitInstanceFromGeneratorWrapper, GetCoreIRModule


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
    name = "MapParallel_n{}_op${}".format(str(numInputs), str(type(op)))
    moduleToReturn = CircuitInstanceFromGeneratorWrapper(cirb, "aetherlinglib", "mapParallel",
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         name,
                                                         {"numInputs": numInputs,
                                                          "operator": GetCoreIRModule(cirb, op)})
    return moduleToReturn


def MapSequential(cirb: CoreIRBackend, numInputs: int, op: Circuit) -> Circuit:
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
    if op.defn.instances.__contains__(op):
        op.defn.instances.remove(op)
    name = "MapParallel_n{}_op${}".format(str(numInputs), str(type(op)))
    moduleToReturn = CircuitInstanceFromGeneratorWrapper(cirb, "aetherlinglib", "mapSequential",
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         name,
                                                         {"numInputs": numInputs,
                                                          "operator": GetCoreIRModule(cirb, op)})
    return moduleToReturn

