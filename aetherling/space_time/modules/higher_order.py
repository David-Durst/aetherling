from aetherling.space_time.space_time_types import *
from aetherling.space_time.type_helpers import valid_ports, strip_tseq_1_0_sseq_1
from aetherling.space_time.nested_counters import DefineNestedCounters
from aetherling.modules.map_fully_parallel_sequential import DefineNativeMapParallel
from aetherling.modules.reduce import DefineReduceParallel, DefineReduceSequential, tupleToTwoInputsForReduce
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
def DefineMap_S(n: int, op: DefineCircuitKind, has_valid=True) -> DefineCircuitKind:
    assert op.binary_op == False
    map_s = DefineNativeMapParallel(n, op, True, has_ready=False, has_valid=has_valid)
    map_s.binary_op = False
    map_s.st_in_t = ST_SSeq(n, op.st_in_t)
    map_s.st_out_t = ST_SSeq(n, op.st_out_t)
    return map_s

@cache_definition
def DefineMap2_S(n: int, op: DefineCircuitKind, has_valid=True) -> DefineCircuitKind:
    assert op.binary_op == True
    map_s = DefineNativeMapParallel(n, op, True, has_ready=False, has_valid=has_valid)
    map_s.binary_op = True
    map_s.st_in_t = [ST_SSeq(n, op.st_in_t[0]), ST_SSeq(n, op.st_in_t[1])]
    map_s.st_out_t = ST_SSeq(n, op.st_out_t)
    return map_s

@cache_definition
def DefineMap_T(n: int, inv: int, op: DefineCircuitKind) -> DefineCircuitKind:
    assert op.binary_op == False
    return DefineMap_T_1_or_2(n, inv, op, True)

@cache_definition
def DefineMap2_T(n: int, inv: int, op: DefineCircuitKind) -> DefineCircuitKind:
    assert op.binary_op == True
    return DefineMap_T_1_or_2(n, inv, op, False)

@cache_definition
def DefineMap_T_1_or_2(n: int, inv: int, op: DefineCircuitKind, is_unary: bool) -> DefineCircuitKind:
    class _Map_T(Circuit):
        name = "Map_T_n{}_i{}_op{}".format(str(n), str(inv), cleanName(str(op)))
        op_num_ports = len(op.IO.Decl) // 2
        op_port_names = op.IO.Decl[::2]
        op_port_types = op.IO.Decl[1::2]
        non_clk_ports = []
        for i in range(op_num_ports):
            if (op_port_names[i] is 'CLK'):
                continue
            non_clk_ports += [op_port_names[i], op_port_types[i]]
        IO = non_clk_ports + ClockInterface(has_ce=False, has_reset=False)
        binary_op = not is_unary
        if is_unary:
            st_in_t = ST_TSeq(n, inv, op.st_in_t)
        else:
            st_in_t = [ST_TSeq(n, inv, op.st_in_t[0]), ST_TSeq(n, inv, op.st_in_t[1])]
        st_out_t = ST_TSeq(n, inv, op.st_out_t)

        @classmethod
        def definition(cls):
            op_instance = op()
            for i in range(cls.op_num_ports):
                port_name = cls.op_port_names[i]
                wire(getattr(cls, port_name), getattr(op_instance, port_name))
    return _Map_T

@cache_definition
def DefineReduce_S(n: int, op: DefineCircuitKind, has_valid=False) -> DefineCircuitKind:
    class _Reduce_S(Circuit):
        assert type(op.st_in_t) == ST_Atom_Tuple
        name = "Reduce_S_n{}_op{}".format(str(n), cleanName(str(op)))
        atom_type = op.st_in_t.t0
        st_in_t = ST_SSeq(n, atom_type)
        st_out_t = ST_SSeq(1, atom_type)
        IO = ['I', In(st_in_t.magma_repr()), 'O', Out(st_out_t.magma_repr())]
        if has_valid:
            IO += valid_ports

        @classmethod
        def definition(cls):
            op_renamed = tupleToTwoInputsForReduce(op)
            reduce = DefineReduceParallel(n, op_renamed)()
            wire(cls.I, reduce.I)
            wire(cls.O[0], reduce.O)
            if has_valid:
                wire(cls.valid_up, cls.valid_down)
    return _Reduce_S

@cache_definition
def DefineReduce_T(n: int, i: int, op: DefineCircuitKind) -> DefineCircuitKind:
    class _Reduce_T(Circuit):
        # second case handles partially parallel generated code where reduce over a map_s 1
        assert type(op.st_in_t) == ST_Atom_Tuple or \
               (type(op.st_in_t) == ST_SSeq and op.st_in_t.n == 1 and type(op.st_in_t.t) == ST_Atom_Tuple)
        name = "Reduce_T_n{}_i{}_op{}".format(str(n), str(i), cleanName(str(op)))
        atom_type = op.st_in_t.t0 if type(op.st_in_t) == ST_Atom_Tuple else ST_SSeq(1, op.st_in_t.t.t0)
        binary_op = False
        st_in_t = ST_TSeq(n, i, atom_type)
        st_out_t = ST_TSeq(1, n+i-1, atom_type)
        IO = ['I', In(st_in_t.magma_repr()), 'O', Out(st_out_t.magma_repr())] + valid_ports

        @classmethod
        def definition(cls):
            op_renamed = tupleToTwoInputsForReduce(op, type(op.st_in_t) == ST_SSeq)
            reduce = DefineReduceSequential(n, op_renamed, has_ce=True)()
            enable_counter = DefineNestedCounters(ST_TSeq(n, i, ST_Int()),
                                                  has_last=False,
                                                  has_ce=True)()
            wire(enable_counter.valid, reduce.CE)
            wire(cls.valid_up, enable_counter.CE)
            if type(op.st_in_t) == ST_Atom_Tuple:
                wire(cls.I, reduce.I)
                wire(cls.O, reduce.out)
            else:
                wire(cls.I[0], reduce.I)
                wire(cls.O[0], reduce.out)

            # valid output after first full valid input collected
            valid_delay = InitialDelayCounter(n)
            wire(cls.valid_up, valid_delay.CE)
            wire(cls.valid_down, valid_delay.valid)

            # ignore inner reduce ready and valid
            wire(reduce.valid, TermAnyType(Bit).I)
            wire(reduce.ready, TermAnyType(Bit).I)
    return _Reduce_T