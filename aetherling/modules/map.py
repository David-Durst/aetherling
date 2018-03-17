from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import ModuleFromGeneratorWrapper
from coreir.module import Module

__all__ = ["MapParallel", "MapSequential"]

def MapParallel(cirb: CoreIRBackend, numInputs: int, op: Module):
    """
    Map an operation over numInputs inputs in one clock cycle
    Aetherling Type: {1, T[numInputs]} -> {1, S[numInputs]}

    :param cirb: The CoreIR backend currently be used
    :param numInputs: The number of input elements
    :param op: The operator (the coreir module) to map over the elements. It should
    s.t. Op : T -> S
    :return: A module with the following ports:
    I : In(Array(numInputs, T))
    O : Out(Array(numInputs, S))
    """
    moduleToReturn = ModuleFromGeneratorWrapper(cirb, "aetherlinglib", "mapParallel",
                                                ["commonlib", "mantle", "coreir", "global"],
                                                {"numInputs": numInputs, "operator": op})
    return moduleToReturn

def MapSequential(cirb: CoreIRBackend, numInputs: int, op: Module):
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
    moduleToReturn = ModuleFromGeneratorWrapper(cirb, "aetherlinglib", "mapSequential",
                                                ["commonlib", "mantle", "coreir", "global"],
                                                {"numInputs": numInputs, "operator": op})
    return moduleToReturn