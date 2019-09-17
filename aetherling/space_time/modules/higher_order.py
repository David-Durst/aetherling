from aetherling.space_time.space_time_types import *
from aetherling.space_time.nested_counters import DefineNestedCounters
from aetherling.modules.map_fully_parallel_sequential import DefineNativeMapParallel
from aetherling.modules.reduce import DefineReduceParallel, DefineReduceSequential, renameCircuitForReduce
from aetherling.modules.initial_delay_counter import InitialDelayCounter
from aetherling.modules.term_any_type import TermAnyType
from aetherling.helpers.nameCleanup import cleanName
from mantle.coreir.arith import *
from mantle.coreir.logic import *
from mantle.coreir.compare import *
from mantle.coreir import DefineCoreirConst
from magma.circuit import DefineCircuitKind

int_width = ST_Int().magma_repr().size()
bit_width = ST_Bit().magma_repr().size()

@cache_definition
def DefineMap_S(n: int, op: DefineCircuitKind) -> DefineCircuitKind:
    assert op.binary_op == False
    map_s = DefineNativeMapParallel(n, op, True)
    map_s.binary_op = False
    map_s.st_in_t = ST_SSeq(n, op.st_in_t)
    map_s.st_out_t = ST_SSeq(n, op.st_out_t)
    return map_s

@cache_definition
def DefineMap_T(n: int, inv: int, op: DefineCircuitKind) -> DefineCircuitKind:
    assert op.binary_op == False
    class _Map_T(Circuit):
        name = "Map_T_n{}_i{}_op{}".format(str(n), str(inv), cleanName(str(op)))
        op_num_ports = len(op.IO.Decl) // 2
        op_port_names = op.IO.Decl[::2]
        op_port_types = op.IO.Decl[1::2]
        non_clk_ports = [[op_port_names[i], op_port_types[i]]
                         for i in range(op_num_ports) if op_port_names[i] != 0]
        has_valid = 'valid_in' in op_port_names
        IO = non_clk_ports + ClockInterface(has_ce=False, has_reset=False)
        binary_op = False
        st_in_t = ST_TSeq(n, inv, op.st_in_t)
        st_out_t = ST_TSeq(n, inv, op.st_out_t)

        @classmethod
        def definition(cls):
            op_instance = op()
            for i in range(cls.op_num_ports):
                port_name = cls.op_port_names[i]
                wire(getattr(cls, port_name), getattr(op_instance, port_name))
            if cls.has_valid:
                wire(cls.valid_in, op_instance.CE)
                wire(op_instance.valid_out, cls.valid_out)
    return _Map_T

@cache_definition
def DefineReduce_S(n: int, op: DefineCircuitKind) -> DefineCircuitKind:
    class _Reduce_S(Circuit):
        assert type(op.st_in_t) == ST_Atom_Tuple
        name = "Reduce_S_n{}_op{}".format(str(n), cleanName(str(op)))
        atom_type = op.st_in_t.t0
        st_in_t = ST_SSeq(n, atom_type)
        st_out_t = ST_SSeq(1, atom_type)
        IO = ['I', st_in_t.magma_repr(), 'O', st_out_t.magma_repr()]

        @classmethod
        def definition(cls):
            op_renamed = renameCircuitForReduce(op)
            reduce = DefineReduceParallel(n, op_renamed)()
            wire(cls.I, reduce.I)
            wire(cls.O[0], reduce.O)
    return _Reduce_S

@cache_definition
def DefineReduce_T(n: int, i: int, op: DefineCircuitKind) -> DefineCircuitKind:
    class _Reduce_T(Circuit):
        assert type(op.st_in_t) == ST_Atom_Tuple
        name = "Reduce_T_n{}_i{}_op{}".format(str(n), str(i), cleanName(str(op)))
        atom_type = op.st_in_t.t0
        binary_op = False
        st_in_t = ST_TSeq(n, i, atom_type)
        st_out_t = ST_TSeq(1, n+i-1, atom_type)
        IO = ['I', st_in_t.magma_repr(), 'O', st_out_t.magma_repr(),
              'valid_in', In(Bit), 'valid_out', Out(Bit)]

        @classmethod
        def definition(cls):
            op_renamed = renameCircuitForReduce(op)
            reduce = DefineReduceSequential(n, op_renamed, has_ce=True)()
            enable_counter = DefineNestedCounters(ST_TSeq(n, i, ST_Int()),
                                                  has_last=False,
                                                  has_ce=True)
            wire(enable_counter.valid, reduce.CE)
            wire(cls.valid_in, enable_counter.CE)
            wire(cls.I, reduce.I)
            wire(cls.O, reduce.O)

            # valid output after first full valid input collected
            valid_delay = InitialDelayCounter(n)
            wire(cls.valid_in, valid_delay.CE)
            wire(cls.valid_out, valid_delay.valid)

            # ignore inner reduce ready and valid
            wire(reduce.valid, TermAnyType(Bit).I)
            wire(reduce.ready, TermAnyType(Bit).I)
    return _Reduce_T
