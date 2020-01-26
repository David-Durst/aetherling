from magma import *
from magma.circuit import DefineCircuitKind, Circuit
from magma.frontend.coreir_ import CircuitInstanceFromGeneratorWrapper, GetCoreIRModule, DefineCircuitFromGeneratorWrapper, \
    GetCoreIRBackend
from mantle.coreir import DefineCoreirConst
from mantle.common.countermod import SizedCounterModM
from ..helpers.nameCleanup import cleanName
from aetherling.helpers.magma_helpers import *
from aetherling.modules.register_any_type import DefineRegisterAnyType
from aetherling.modules.mux_any_type import DefineMuxAnyType
from aetherling.modules.counter import AESizedCounterModM
from aetherling.modules.initial_delay_counter import InitialDelayCounter
from math import ceil

@cache_definition
def DefineReduceParallelWithIdentity(numInputs: int, op: Circuit) -> Circuit:
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
        data: Array[numInputs, T],
        identity: T
    })
    out: Out(T)
    """
    cirb = GetCoreIRBackend()
    if op.is_instance and op.defn.instances.__contains__(op):
        op.defn.instances.remove(op)
    name = "ReduceParallel_n{}_op{}".format(str(numInputs), cleanName(str(op)))
    moduleToReturn = DefineCircuitFromGeneratorWrapper(cirb, "aetherlinglib", "reduceParallel", name,
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         {"numInputs": numInputs,
                                                          "operator": GetCoreIRModule(cirb, op)})
    return moduleToReturn

@cache_definition
def DefineReduceParallel(numInputs: int, opDef: Circuit) -> Circuit:
    """
    Reduce multiple numInputs into one in one clock cycle.
    Aetherling Type: ({1, T[numInputs]} -> {1, T}, 1)

    :param cirb: The CoreIR backend currently be used
    :param numInputs: The number of input elements
    :param op: The operator (the magma circuit) to map over the elements. It should have type T -> T -> T,
    with input ports in0 and in1.
    :return: A module with ports:
    I: Array[numInputs, T]
    out: Out(T)
    """
    class _ReduceParallel(Circuit):
        name = "ReduceParallel_n{}".format(str(numInputs))
        token_type = type(opDef.interface.outputs()[0])

        IO = ['I', In(Array[numInputs, token_type]), 'O', Out(token_type)]

        @classmethod
        def definition(cls):
            if numInputs == 1:
                wire(cls.I[0], cls.O)
                return
            ops = [opDef() for _ in range(numInputs - 1)]
            unflattened_ports = [[get_in0(op), get_in1(op)] for op in ops]
            unwired_in_ports = set([port for op_ports in unflattened_ports for port in op_ports])

            # wire ops into tree
            for i in range(numInputs - 1):
                if i == 0:
                    wire(get_out(ops[i]), cls.O)
                elif i % 2 == 0:
                    wire(get_out(ops[i]), get_in0(ops[i // 2 - 1]))
                    unwired_in_ports.discard(get_in0(ops[i // 2 - 1]))
                else:
                    wire(get_out(ops[i]), get_in1(ops[ceil(i / 2) - 1]))
                    unwired_in_ports.discard(get_in1(ops[ceil(i / 2) - 1]))

            # wire inputs to tree
            for (i, op_in_port) in zip(range(numInputs), unwired_in_ports):
                wire(cls.I[i], op_in_port)


    return _ReduceParallel

def ReduceParallel(numInputs: int, op: Circuit) -> Circuit:
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
    I: Array[numInputs, T]
    out: Out(T)
    """
    return DefineReduceParallel(numInputs, op)()

@cache_definition
def DefineReduceSequential(numInputs: int, opDef: Circuit, has_ce=False) -> Circuit:
    """
    Reduce multiple numInputs into one in numInputs clock cycles.
    Aetherling Type: ({numInputs, T} -> {1, T}, numInputs)

    :param numInputs: The number of input elements
    :param op: The operator definition (the magma circuit) to map over the elements. It should have type T -> T -> T,
    with input ports in0 and in1 and output port out. Use renameCircuitForReduce to do the renaming.
    :return: A module with ports:
    I: In(T)
    out: Out(T)
    ready: Out(Bit)
    valid: Out(Bit)

    if has_ce == True:
    CE : In(Enable)
    """
    class _ReduceSequential(Circuit):
        name = "ReduceSequential_n{}_ce{}".format(str(numInputs), str(has_ce))
        token_type = type(opDef.interface.outputs()[0])

        IO = ['I', In(token_type), 'out', Out(token_type), 'ready', Out(Bit),
              'valid', Out(Bit)] + ClockInterface(has_ce=has_ce)

        @classmethod
        def definition(cls):
            length_counter = AESizedCounterModM(numInputs, has_ce=has_ce, cout=True)
            reg_input_mux = DefineMuxAnyType(cls.token_type, 2)()
            op_instance = opDef()
            accumulatorRegister = DefineRegisterAnyType(cls.token_type, has_ce=has_ce)()
            if has_ce:
                wire(cls.CE, accumulatorRegister.CE)
                wire(cls.CE, length_counter.CE)

            wire(cls.I, get_in0(op_instance))
            wire(accumulatorRegister.O, get_in1(op_instance))

            # use reduce input for register on first clock,
            # otherwise use op output
            wire(get_out(op_instance),reg_input_mux.data[0])
            wire(cls.I,reg_input_mux.data[1])
            wire(0 == length_counter.O, reg_input_mux.sel[0])
            wire(reg_input_mux.out, accumulatorRegister.I)

            wire(get_out(op_instance), cls.out)

            wire(1, cls.ready)
            # valid when COUT == 1 as register contains sum of all inputs
            wire(length_counter.COUT == 1, cls.valid)

    return _ReduceSequential

def ReduceSequential(numInputs: int, opDef: Circuit, has_ce = False) -> Circuit:
    """
    Reduce multiple numInputs into one in numInputs clock cycles.
    Aetherling Type: ({numInputs, T} -> {1, T}, numInputs)

    :param numInputs: The number of input elements
    :param op: The operator definition (the magma circuit) to map over the elements. It should have type T -> T -> T,
    with input ports in0 and in1 and output port out. Use renameCircuitForReduce to do the renaming
    :return: A module with ports:
    I: In(T)
    out: Out(T)
    ready: Out(Bit)
    valid: Out(Bit)

    if has_ce == True:
    CE : In(Enable)
    """
    return DefineReduceSequential(numInputs, opDef, has_ce)()

@cache_definition
def DefineReducePartiallyParallel(
        numInputs: int,
        parallelism: int,
        op: Circuit,
        has_ce = False) -> Circuit:
    """
    Reduce multiple numInputs into one over numInputs/parallelism clock cycles, parallelism per clock cycle
    Aetherling Type: {1, T[numInputs]} -> {numInputs/parallelism, S[parallelism]}

    :param numInputs: The total number of input elements
    :param numInputs: The number of input elements to process each clock cycle
    :param op: The operator definition (the magma circuit) to map over the elements. It should have type T -> T -> T,
    with input ports in0 and in1.
    
    Restrictions:
    1. numInputs % parallelism == 0


    :return: A module with all the ports of op, except the input is parallelized into an array of
      length parallelism. The module also requires an identity constant corresponding to the 
      op (e.g. 0 for Add, 1 for Multiply) formatted as a CoreirConstant, and includes a write enable 
      port on the input and a valid bit for the output. For an op with input port I and output O, 
      the ports will be:
      I : In(Array[parallelism, T]
      O : Out(T)
      ready :  Out(Bit)
      valid :  Out(Bit)

      if has_ce == True:
      CE : In(Enable)
    """

    class _ReducePartiallyParallel(Circuit):
        # numInputs/parallelism is the number of clock cycles to reduce over,
        # so must be an integer
        if numInputs % parallelism != 0:
                raise Exception("Reduce Partially Parallel has invalid "
                                "parameters: numInputs {} not divisible by "
                                "parallelism {}".format(numInputs, parallelism))

        name = "Reduce_n{}_p{}_op{}".format(str(numInputs), str(parallelism), 
            cleanName(op.name))
        
        token_type = type(renameCircuitForReduce(op).in0)

        IO = ['I', In(Array[parallelism, token_type]),
              'O', Out(token_type),
              'ready', Out(Bit),
              'valid', Out(Bit)] + ClockInterface(has_ce=has_ce)

        @classmethod
        def definition(rpp):
            num_clocks = numInputs // parallelism

            # reduce Parallel
            reducePar = ReduceParallel(parallelism, renameCircuitForReduce(op))

            # reduce sequential
            reduceSeq = ReduceSequential(num_clocks, renameCircuitForReduce(op))

            # top level input fed to reduce parallel input
            wire(rpp.I, reducePar.I)

            # reduce parallel output fed to reduce sequential input
            wire(reducePar.O, reduceSeq.I)

            # output of reduce sequential fed to top level output
            wire(reduceSeq.out, rpp.O)
            wire(reduceSeq.ready, rpp.ready)
            wire(reduceSeq.valid, rpp.valid)
            if has_ce:
                wire(reduceSeq.CE, rpp.CE)

    return _ReducePartiallyParallel

def ReducePartiallyParallel(numInputs: int, parallelism: int,
                         op: Circuit, has_ce=False):
    return DefineReducePartiallyParallel(numInputs, parallelism, op)()

def get_in0(op: Circuit):
    return op.interface.inputs()[0]

def get_in1(op: Circuit):
    return op.interface.inputs()[1]

def get_out(op: Circuit):
    return op.interface.outputs()[0]

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
    inputs = getInputPorts(opDef.IO)
    output = getOutputPorts(opDef.IO)
    assert len(inputs) == 2 # must have only 2 inputs
    assert len(output) == 1 # must have only 1 output
    assert inputs[0][1] == inputs[1][1] # all inputs and outputs must be same type
    assert In(output[0][1]) == inputs[0][1] # need to do Out instead of in conversion as types reversed here

    class _RenamedCircuit(Circuit):
        name = "renamedForReduce"
        IO = ["in0", In(inputs[0][1]), "in1", In(inputs[1][1]), "out", Out(inputs[0][1])]

        @classmethod
        def definition(renamedCircuit):
            op = opDef()
            wire(getattr(op, inputs[0][0]), renamedCircuit.in0)
            wire(getattr(op, inputs[1][0]), renamedCircuit.in1)
            wire(getattr(op, output[0][0]), renamedCircuit.out)
            return renamedCircuit

    return _RenamedCircuit


@cache_definition
def tupleToTwoInputsForReduce(opDef: DefineCircuitKind, extra_sseq=0) -> DefineCircuitKind:
    """
    Given an operation definition with one input port and one output port
    where the input port is a tuple of type t x t and the output port is of type t,
    return an circuit definition with ports name in0, in1, and out
    that reduce needs.
    :param op: A circuit definition with ports In(t x t) and Out(t).
    :return: A circuit definition with the ports renamed
    in0: In(t)
    in1: In(t)
    out: Out(t)
    """
    interface = opDef.IO()
    inputs = interface.inputs()
    output = interface.outputs()
    assert len(inputs) == 1 # must have only 2 inputs
    assert len(output) == 1 # must have only 1 output
    # remove extra sseq  + 1 for the outer array of num ports
    striped_inputs = inputs
    striped_output = output
    for i in range(extra_sseq+1):
        striped_inputs = striped_inputs[0]
        striped_output = striped_output[0]
    assert type(striped_inputs[0]) == type(striped_inputs[1]) # all inputs and outputs must be same type
    assert In(type(striped_output)) == type(striped_inputs[0]) # need to do Out instead of in conversion as types reversed here

    in0_type = type(striped_inputs[0])
    in1_type = type(striped_inputs[1])
    for i in range(extra_sseq):
        in0_type = Array[1, in0_type]
        in1_type = Array[1, in1_type]

    class _RenamedCircuit(Circuit):
        name = "renamedForReduce"
        IO = ["in0", in0_type, "in1", in1_type, "out", Out(type(output[0]))]

        @classmethod
        def definition(renamedCircuit):
            op = opDef()
            interface_instance = op.IO()
            inputs_instance = interface_instance.inputs()
            output_instance = interface_instance.outputs()
            op_in = getattr(op, inputs_instance[0].name.name)
            op_out = getattr(op, output_instance[0].name.name)
            cls_in0 = renamedCircuit.in0
            cls_in1 = renamedCircuit.in1
            cls_out = renamedCircuit.out
            for i in range(extra_sseq):
                op_in = op_in[0]
                op_out = op_out[0]
                cls_in0 = cls_in0[0]
                cls_in1 = cls_in1[0]
                cls_out = cls_out[0]
            wire(op_in[0], cls_in0)
            wire(op_in[1], cls_in1)
            wire(op_out, cls_out)
            return renamedCircuit

    return _RenamedCircuit

