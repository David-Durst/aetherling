from magma import *
from magma.circuit import DefineCircuitKind
from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import DefineCircuitFromGeneratorWrapper, GetCoreIRModule
from ..helpers.nameCleanup import cleanName

def DefineSDFMapParallel(cirb: CoreIRBackend, numInputs: int, op: DefineCircuitKind) -> DefineCircuitKind:
    """
    Creates a MapParallel circuit defintion wrapped in an SDF
    :param cirb:
    :param numInputs:
    :param op:
    :return:
    """

def DefineMapParallel(cirb: CoreIRBackend, numInputs: int, op: DefineCircuitKind) -> DefineCircuitKind:
    """
    Map an operation over numInputs inputs in one clock cycle
    Aetherling Type: {1, T[numInputs]} -> {1, S[numInputs]}
    This returns a circuit definition.

    :param cirb: The CoreIR backend currently be used
    :param numInputs: The number of input elements
    :param op: The operator (the magma circuit) to map over the elements. It should have type T -> S
    :return: A module with all the ports of op, except that they will be converted to arrays
     of original type, with each array of length numInputs. For example, if op has
     one input port I and one output O, the ports will be:
    I : In(Array(numInputs, T))
    O : Out(Array(numInputs, S))
    """
    if hasattr(op, 'is_instance') and op.is_instance and op.defn.instances.__contains__(op):
        op.defn.instances.remove(op)
    name = "MapParallel_n{}_op{}".format(str(numInputs), cleanName(str(op)))
    definitionToReturn = DefineCircuitFromGeneratorWrapper(cirb, "aetherlinglib", "mapParallel", name,
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         {"numInputs": numInputs,
                                                          "operator": GetCoreIRModule(cirb, op)})
    return definitionToReturn

def MapParallel(cirb: CoreIRBackend, numInputs: int, op: DefineCircuitKind) -> Circuit:
    """
    Map an operation over numInputs inputs in one clock cycle
    Aetherling Type: {1, T[numInputs]} -> {1, S[numInputs]}
    This returns a circuit instance, an instance of a circuit definition

    :param cirb: The CoreIR backend currently be used
    :param numInputs: The number of input elements
    :param op: The operator (the magma circuit) to map over the elements. It should have type T -> S
    :return: A module with all the ports of op, except that they will be converted to arrays
     of original type, with each array of length numInputs. For example, if op has
     one input port I and one output O, the ports will be:
    I : In(Array(numInputs, T))
    O : Out(Array(numInputs, S))
    """
    return DefineMapParallel(cirb, numInputs, op)()

def DefineNativeMapParallel(cirb: CoreIRBackend, numInputs: int, op: DefineCircuitKind) -> DefineCircuitKind:
    class _Map(Circuit):
        name = "NativeMapParallel_n{}_op{}".format(str(numInputs), cleanName(str(op)))
        IO = []
        for i in range(len(op.IO.Decl) // 2):
            IO += [op.IO.Decl[i*2], Array(numInputs, op.IO.Decl[i*2+1])]
        @classmethod
        def definition(cls):
            for i in range(numInputs):
                op_instance = op()
                for j in range(len(op.IO.Decl) // 2):
                    port_name = op.IO.Decl[j*2]
                    wire(getattr(cls, port_name)[i], getattr(op_instance, port_name))
    return _Map

def DefineMapSequential(cirb: CoreIRBackend, numInputs: int, op: DefineCircuitKind) -> DefineCircuitKind:
    """
    Map an operation over numInputs inputs over numInputs cycles.
    Note: the entire inputs must be delivered on the first cycle.
    There is no point in implementing the streaming version of this operation
    as that is just the module op.
    Aetherling Type: {1, T[numInputs]} -> {1, S[numInputs]}
    This returns a circuit definition.

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
    if op.is_instance and op.defn.instances.__contains__(op):
        op.defn.instances.remove(op)
    name = "MapSequential_n{}_op{}".format(str(numInputs), cleanName(str(op)))
    definitionToReturn = DefineCircuitFromGeneratorWrapper(cirb, "aetherlinglib", "mapSequential", name,
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         {"numInputs": numInputs,
                                                          "operator": GetCoreIRModule(cirb, op)})
    return definitionToReturn


def MapSequential(cirb: CoreIRBackend, numInputs: int, op: DefineCircuitKind) -> Circuit:
    """
    Map an operation over numInputs inputs over numInputs cycles.
    Note: the entire inputs must be delivered on the first cycle.
    There is no point in implementing the streaming version of this operation
    as that is just the module op.
    Aetherling Type: {1, T[numInputs]} -> {1, S[numInputs]}
    This returns a circuit instance, an instance of a circuit definition

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
    return DefineMapSequential(cirb, numInputs, op)()

