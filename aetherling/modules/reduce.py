from magma import *
from magma.circuit import DefineCircuitKind
from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import CircuitInstanceFromGeneratorWrapper, GetCoreIRModule
from ..helpers.nameCleanup import cleanName

def ReduceParallel(cirb: CoreIRBackend, numInputs: int, op: Circuit) -> Circuit:
    """
    Reduce multiple numInputs into one in one clock cycle.
    This uses a reduction tree but can handle numInputs that aren't powers of 2.
    It does this using the identity element to fill in inputs to the tree that aren't used.
    Aetherling Type: ({1, T[numInputs]} -> {1, T}, 1)

    :param cirb: The CoreIR backend currently be used
    :param numInputs: The number of input elements
    :param op: The operator (the magma circuit) to map over the elements. It should have type T -> T -> T,
    with input ports in0 and in1.
    :return: A module with ports:
    I: In({
        data: Array(numInputs, T),
        identity: T
    })
    out: Out(T)
    """
    if op.is_instance and op.defn.instances.__contains__(op):
        op.defn.instances.remove(op)
    name = "ReduceParallel_n{}_op{}".format(str(numInputs), cleanName(str(type(op))))
    moduleToReturn = CircuitInstanceFromGeneratorWrapper(cirb, "aetherlinglib", "reduceParallel",
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         name,
                                                         {"numInputs": numInputs,
                                                          "operator": GetCoreIRModule(cirb, op)})
    return moduleToReturn


def ReduceSequential(cirb: CoreIRBackend, numInputs: int, op: Circuit) -> Circuit:
    """
    Reduce multiple numInputs into one in numInputs clock cycles.
    Aetherling Type: ({numInputs, T} -> {1, T}, numInputs)

    :param cirb: The CoreIR backend currently be used
    :param numInputs: The number of input elements
    :param op: The operator (the magma circuit) to map over the elements. It should have type T -> T -> T,
    with input ports in0 and in1.
    :return: A module with ports:
    I: In(T)
    out: Out(T)
    valid: Out(Bit)
    """
    if op.is_instance and op.defn.instances.__contains__(op):
        op.defn.instances.remove(op)
    name = "ReduceSequentail_n{}_op{}".format(str(numInputs), cleanName(str(type(op))))
    moduleToReturn = CircuitInstanceFromGeneratorWrapper(cirb, "aetherlinglib", "reduceSequential",
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         name,
                                                         {"numInputs": numInputs,
                                                          "operator": GetCoreIRModule(cirb, op)})
    return moduleToReturn

@cache_definition
def renameCircuitForReduce(opDef: DefineCircuitKind) -> DefineCircuitKind:
    """
    Given an operation definition with two input ports and one output port
    all with same type, return an circuit definition with ports name in0, in1, and out
    that reduce needs.
    :param op: A circuit definition with ports In(T), In(T) and Out(T).
    :return: A circuit definition with the ports renamed
    in0: In(T)
    in1: In(T)
    out: Out(T)
    """
    # note: directionaly reversed for definitions
    inputs = list(zip(opDef.interface.outputargs()[::2], opDef.interface.outputargs()[1::2]))
    output = (opDef.interface.inputargs()[0], opDef.interface.inputargs()[1])
    assert len(inputs) == 2 # must have only 2 inputs
    assert len(opDef.interface.inputargs()) == 2 # must have only 1 name and 1 type in interface list of outputs
    assert type(inputs[0][1]) == type(inputs[1][1]) # all inputs and outputs must be same type
    assert Out(type(output[1])) == type(inputs[0][1]) # need to do Out instead of in conversion as types reversed here

    class _RenamedCircuit(Circuit):
        name = "renamedForReduce_op{}".format(cleanName(str(opDef)))
        typeOfPorts = type(inputs[0][1])
        IO = ["in0", In(typeOfPorts), "in1", In(typeOfPorts), "out", Out(typeOfPorts)]

        @classmethod
        def definition(renamedCircuit):
            op = opDef()
            wire(getattr(op, inputs[0][0]), renamedCircuit.in0)
            wire(getattr(op, inputs[1][0]), renamedCircuit.in1)
            wire(getattr(op, output[0]), renamedCircuit.out)
            return renamedCircuit

    return _RenamedCircuit



