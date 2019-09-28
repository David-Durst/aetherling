from dataclasses import replace
from aetherling.space_time.space_time_types import *
from aetherling.helpers.nameCleanup import cleanName
from magma import *
from magma.circuit import DefineCircuitKind
from aetherling.space_time.type_helpers import get_nested_ports, num_nested_space_layers, valid_ports, \
    strip_tseq_1_0_sseq_1

@cache_definition
def DefinePassthrough(t_in: ST_Type, t_out: ST_Type, has_valid = False) -> DefineCircuitKind:
    """
    Connect two types that are the same except for extra TSeq 1 0 or SSeq 1's

    I : In(SSeq(1, elem_t).magma_repr())
    O : Out(SSeq(n, elem_t).magma_repr())
    if has_valid:
    valid_up : In(Bit)
    valid_down : Out(Bit)
    """
    class _Passthrough(Circuit):
        name = "Passthrough_tIn{}_tOut{}".format(cleanName(str(t_in)), str(cleanName(str(t_out))))
        IO = ['I', In(t_in.magma_repr()), 'O', Out(t_out.magma_repr())]
        if has_valid:
            IO += valid_ports
        @classmethod
        def definition(cls):
            flat_in_ports = get_nested_ports(cls.I, num_nested_space_layers(t_in), [])
            flat_out_ports = get_nested_ports(cls.O, num_nested_space_layers(t_out), [])
            for i_port, o_port in zip(flat_in_ports, flat_out_ports):
                wire(i_port, o_port)
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _Passthrough


