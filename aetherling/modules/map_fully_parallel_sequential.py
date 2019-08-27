from aetherling.helpers.magma_helpers import ready_valid_interface
from magma import *
from magma.circuit import DefineCircuitKind
from magma.frontend.coreir_ import DefineCircuitFromGeneratorWrapper, GetCoreIRModule, GetCoreIRBackend
from ..helpers.nameCleanup import cleanName

@cache_definition
def DefineMapParallel(numInputs: int, op: DefineCircuitKind) -> DefineCircuitKind:
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
    I : In(Array[numInputs, T])
    O : Out(Array[numInputs, S])
    """
    cirb = GetCoreIRBackend()
    if hasattr(op, 'is_instance') and op.is_instance and op.defn.instances.__contains__(op):
        op.defn.instances.remove(op)
    name = "MapParallel_n{}_op{}".format(str(numInputs), cleanName(str(op)))
    definitionToReturn = DefineCircuitFromGeneratorWrapper(cirb, "aetherlinglib", "mapParallel", name,
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         {"numInputs": numInputs,
                                                          "operator": GetCoreIRModule(cirb, op)})
    return definitionToReturn

def MapParallel(numInputs: int, op: DefineCircuitKind) -> Circuit:
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
    I : In(Array[numInputs, T])
    O : Out(Array[numInputs, S])
    """
    return DefineMapParallel(numInputs, op)()

@cache_definition
def DefineNativeMapParallel(numInputs: int, op: DefineCircuitKind, merge_ready_valid_ce_reset: bool = False) -> DefineCircuitKind:
    class _Map(Circuit):
        name = "NativeMapParallel_n{}_op{}".format(str(numInputs), cleanName(str(op)))
        num_ports = len(op.IO.Decl) // 2
        port_names = op.IO.Decl[::2]
        port_types = op.IO.Decl[1::2]

        # booleans for extra ports to add iff merge_ready_valid_ce is true and mapped module has these ports
        ready_valid_ce_port_idxs = []
        add_clk = False
        add_ce = False
        add_rv = False
        add_reset = False
        rv_out_ports = ['ready_up', 'valid_down']
        rv_in_ports = ['valid_up', 'ready_down']
        clock_ports = ['CLK', 'CE', 'RESET']

        if merge_ready_valid_ce_reset:
            for i in range(len(port_names)):
                if port_names[i] in (rv_in_ports + rv_out_ports + clock_ports):
                    ready_valid_ce_port_idxs += [i]
                if port_names[i] == 'CE':
                    add_ce = True
                elif port_names[i] == 'RESET':
                    add_reset = True
                elif port_names[i] == 'CLK':
                    add_clk = True
                else:
                    add_rv = True

        IO = []
        for i in range(len(port_names)):
            # only filtering out ready, valid, or CE if merge_ready_valid is true
            if i not in ready_valid_ce_port_idxs:
                IO += [port_names[i], Array[numInputs, port_types[i]]]

        if add_ce:
            IO += ['CE', In(Enable)]
        if add_reset:
            IO += ['RESET', In(Reset)]
        if add_clk:
            IO += ClockInterface()
        if add_rv:
            IO += ready_valid_interface

        @classmethod
        def definition(cls):
            # will need to and all the outgoing rv ports
            out_ready_ports = []
            out_valid_ports = []
            for i in range(numInputs):
                op_instance = op()
                for j in range(len(cls.port_names)):
                    port_name = cls.port_names[j]
                    if j in cls.ready_valid_ce_port_idxs:
                        # for incoming signals, just wire same map module input to
                        # each instance module
                        if port_name in cls.clock_ports + cls.rv_in_ports:
                            wire(getattr(cls, port_name), getattr(op_instance, port_name))
                        elif port_name == 'ready_up':
                            out_ready_ports += [getattr(op_instance, port_name)]
                        else:
                            out_valid_ports += [getattr(op_instance, port_name)]
                    else:
                        wire(getattr(cls, port_name)[i], getattr(op_instance, port_name))

            if len(out_ready_ports) > 0:
                # wire all ready and valids to out
                anded_ready_ports = out_ready_ports[0]
                for i in range(1, len(out_ready_ports)):
                    anded_ready_ports = anded_ready_ports & out_ready_ports[i]
                wire(cls.ready_up, anded_ready_ports)
            if len(out_valid_ports) > 0:
                anded_valid_ports = out_valid_ports[0]
                for i in range(1, len(out_valid_ports)):
                    anded_valid_ports = anded_valid_ports & out_valid_ports[i]
                wire(cls.valid_down, anded_valid_ports)
    return _Map

@cache_definition
def DefineMapSequential(numInputs: int, op: DefineCircuitKind) -> DefineCircuitKind:
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
     I : In(Array[numInputs, T]
     O : Out(Array[numInputs, S])
    """
    cirb = GetCoreIRBackend()
    if op.is_instance and op.defn.instances.__contains__(op):
        op.defn.instances.remove(op)
    name = "MapSequential_n{}_op{}".format(str(numInputs), cleanName(str(op)))
    definitionToReturn = DefineCircuitFromGeneratorWrapper(cirb, "aetherlinglib", "mapSequential", name,
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         {"numInputs": numInputs,
                                                          "operator": GetCoreIRModule(cirb, op)})
    return definitionToReturn


def MapSequential(numInputs: int, op: DefineCircuitKind) -> Circuit:
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
     I : In(Array[numInputs, T]
     O : Out(Array[numInputs, S])
    """
    return DefineMapSequential(numInputs, op)()

