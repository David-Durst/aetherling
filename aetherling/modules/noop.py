from magma import *
from magma.circuit import DefineCircuitKind
from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import CircuitInstanceFromGeneratorWrapper, GetCoreIRModule, DefineCircuitFromGeneratorWrapper
from ..helpers.nameCleanup import cleanName


def DefineNoop(opDef: Circuit) -> Circuit:
    """
    Given a op definition, create an op with the inputs that match its outputs
    and the outputs of this op are the same as its inputs
    """
    class _Noop(Circuit):
        outputArgs = flatten([name, port] for name, port in opDef.IO.ports.items()
                             if (port.isoutput() and 'ready' not in name and 'valid' not in name))
        name = "Noop_op{}".format(cleanName(str(opDef)))

        input_versions_of_output_names = ["in_" + name for name in outputArgs[::2]]
        input_versions_of_output_types = [In(port) for port in outputArgs[1::2]]

        IO = flatten([[name,port] for (name,port) in
                      zip(input_versions_of_output_names, input_versions_of_output_types)]) + \
            outputArgs

        @classmethod
        def definition(cls):
            for i in range(len(cls.input_versions_of_output_names)):
                wire(getattr(cls, cls.input_versions_of_output_names[i]),
                     getattr(cls, cls.outputArgs[i*2]))

    return _Noop

def Noop(opDef: Circuit) -> Circuit:
    """
    Given a op definition, create an op with the inputs that match its outputs
    and the outputs of this op are the same as its inputs
    """
    return DefineNoop(opDef)

