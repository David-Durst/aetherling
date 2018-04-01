from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import ModuleFromGeneratorWrapper, GetCoreIRModule
from magma.circuit import Circuit, CircuitType

__all__ = ["MapParallel", "MapSequential"]


def MapParallel(cirb: CoreIRBackend, numInputs: int, op: Circuit, opContainer: Circuit) -> Circuit:
    """
    Map an operation over numInputs inputs in one clock cycle
    Aetherling Type: {1, T[numInputs]} -> {1, S[numInputs]}

    :param cirb: The CoreIR backend currently be used
    :param numInputs: The number of input elements
    :param op: The operator (the magma circuit) to map over the elements. It should have type T -> S
    :param opContainer: The magma circuit that contains op, needed for instance management
    :return: A module with the following ports:
    I : In(Array(numInputs, T))
    O : Out(Array(numInputs, S))
    """
    if opContainer.instances.__contains__(op):
        opContainer.instances.remove(op)
    moduleToReturn = ModuleFromGeneratorWrapper(cirb, "aetherlinglib", "mapParallel",
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
    :return: A module with the following ports:
    I : In(Array(numInputs, T)
    O : Out(Array(numInputs, S))
    """
    if opContainer.instances.__contains__(op):
        opContainer.instances.remove(op)
    moduleToReturn = ModuleFromGeneratorWrapper(cirb, "aetherlinglib", "mapSequential",
                                                ["commonlib", "mantle", "coreir", "global"],
                                                {"numInputs": numInputs,
                                                 "operator": GetCoreIRModule(cirb, op)})
    return moduleToReturn