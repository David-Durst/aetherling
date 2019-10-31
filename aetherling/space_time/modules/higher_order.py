from aetherling.space_time.space_time_types import *
from aetherling.space_time.type_helpers import valid_ports, strip_tseq_1_0_sseq_1, strip_tseq_1_n_sseq_1, \
    num_nested_space_layers, replace_atom_tuple_with_t0, time_last_valid
from aetherling.space_time.nested_counters import DefineNestedCounters
from aetherling.modules.map_fully_parallel_sequential import DefineNativeMapParallel
from aetherling.modules.reduce import DefineReduceParallel, DefineReduceSequential, tupleToTwoInputsForReduce
from aetherling.modules.initial_delay_counter import InitialDelayCounter
from aetherling.modules.term_any_type import TermAnyType
from aetherling.helpers.nameCleanup import cleanName, undup_
from magma import *
from mantle.coreir.arith import *
from mantle.coreir.logic import *
from mantle.coreir.compare import *
from mantle.coreir import DefineCoreirConst, DefineRegister
from magma.circuit import DefineCircuitKind
from aetherling.modules.register_any_type import DefineRegisterAnyType

int_width = ST_Int().magma_repr().size()
bit_width = ST_Bit().magma_repr().size()

@cache_definition
def DefineMap_S(n: int, op: DefineCircuitKind, has_valid=True) -> DefineCircuitKind:
    assert op.binary_op == False
    map_s = DefineNativeMapParallel(n, op, True, has_ready=False, has_valid=has_valid)
    map_s.binary_op = False
    map_s.st_in_t = [ST_SSeq(n, op.st_in_t[0])]
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
        name = undup_("Map_T_n{}_i{}_op{}".format(str(n), str(inv), cleanName(str(op))).replace("__","_"))
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
            st_in_t = [ST_TSeq(n, inv, op.st_in_t[0])]
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
        assert type(strip_tseq_1_n_sseq_1(op.st_in_t[0])) == ST_Atom_Tuple
        name = "Reduce_S_n{}_op{}".format(str(n), cleanName(str(op)))
        binary_op = False
        st_in_t = [ST_SSeq(n, replace_atom_tuple_with_t0(op.st_in_t[0]))]
        st_out_t = ST_SSeq(1, op.st_out_t)
        IO = ['I', In(st_in_t[0].magma_repr()), 'O', Out(st_out_t.magma_repr())] + ClockInterface()
        if has_valid:
            IO += valid_ports

        @classmethod
        def definition(cls):
            op_renamed = tupleToTwoInputsForReduce(op, num_nested_space_layers(cls.st_in_t[0]) - 1)
            reduce = DefineReduceParallel(n, op_renamed)()
            wire(cls.I, reduce.I)
            red_reg = DefineRegisterAnyType(cls.st_out_t.magma_repr())()
            wire(reduce.O, red_reg.I[0])
            wire(cls.O, red_reg.O)

            if has_valid:
                valid_reg = DefineRegister(1)()
                wire(cls.valid_up, valid_reg.I[0])
                wire(valid_reg.O[0], cls.valid_down)
    return _Reduce_S

@cache_definition
def DefineReduce_T(n: int, i: int, op: DefineCircuitKind) -> DefineCircuitKind:
    class _Reduce_T(Circuit):
        # second case handles partially parallel generated code where reduce over a map_s 1
        assert type(strip_tseq_1_n_sseq_1(op.st_in_t[0])) == ST_Atom_Tuple
        name = "Reduce_T_n{}_i{}_op{}".format(str(n), str(i), cleanName(str(op)))
        binary_op = False
        st_in_t = [ST_TSeq(n, i, replace_atom_tuple_with_t0(op.st_in_t[0]))]
        st_out_t = ST_TSeq(1, n+i-1, op.st_out_t)
        IO = ['I', In(st_in_t[0].magma_repr()), 'O', Out(st_out_t.magma_repr())] + valid_ports + ClockInterface()

        @classmethod
        def definition(cls):
            op_renamed = tupleToTwoInputsForReduce(op, num_nested_space_layers(cls.st_in_t[0]))
            reduce = DefineReduceSequential(n, op_renamed, has_ce=True)()
            enable_counter = DefineNestedCounters(cls.st_in_t[0],
                                                  has_last=False,
                                                  has_ce=True)()
            wire(enable_counter.valid, reduce.CE)
            wire(cls.valid_up, enable_counter.CE)
            wire(cls.I, reduce.I)

            red_reg = DefineRegisterAnyType(cls.st_out_t.magma_repr())()
            wire(reduce.out, red_reg.I)
            wire(cls.O, red_reg.O)

            # valid output after first full valid input collected
            valid_delay = InitialDelayCounter(time_last_valid(cls.st_in_t[0]) + 1)
            wire(cls.valid_up, valid_delay.CE)
            wire(cls.valid_down, valid_delay.valid)

            # ignore inner reduce ready and valid
            wire(reduce.valid, TermAnyType(Bit).I)
            wire(reduce.ready, TermAnyType(Bit).I)
    return _Reduce_T

@cache_definition
def DefineAdd_1_S(op: DefineCircuitKind, has_valid=False) -> DefineCircuitKind:
    class _Add_1_S(Circuit):
        name = "Add_1_S_op{}".format( cleanName(str(op)))
        binary_op = False
        st_in_t = op.st_in_t
        st_out_t = ST_SSeq(1, op.st_out_t)
        IO = ['I', In(st_in_t[0].magma_repr()), 'O', Out(st_out_t.magma_repr())]
        if has_valid:
            IO += valid_ports

        @classmethod
        def definition(cls):
            op_inst = op()
            wire(cls.I, op_inst.I)
            wire(cls.O[0], op_inst.O)

            if has_valid:
                wire(cls.valid_up, op_inst.valid_up)
                wire(cls.valid_down, op_inst.valid_down)

    return _Add_1_S

@cache_definition
def DefineRemove_1_S(op: DefineCircuitKind, has_valid=False) -> DefineCircuitKind:
    class _Remove_1_S(Circuit):
        name = "Remove_1_S_op{}".format( cleanName(str(op)))
        binary_op = False
        st_in_t = [ST_SSeq(1, op.st_in_t[0])]
        st_out_t = op.st_out_t
        IO = ['I', In(st_in_t[0].magma_repr()), 'O', Out(st_out_t.magma_repr())]
        if has_valid:
            IO += valid_ports

        @classmethod
        def definition(cls):
            op_inst = op()
            wire(cls.I[0], op_inst.I)
            wire(cls.O, op_inst.O)

            if has_valid:
                wire(cls.valid_up, op_inst.valid_up)
                wire(cls.valid_down, op_inst.valid_down)

    return _Remove_1_S

@cache_definition
def DefineAdd_1_0_T(op: DefineCircuitKind, has_valid=False) -> DefineCircuitKind:
    class _Add_1_0_T(Circuit):
        name = "Add_1_S_op{}".format( cleanName(str(op)))
        binary_op = False
        st_in_t = op.st_in_t
        st_out_t = ST_TSeq(1, 0, op.st_out_t)
        IO = ['I', In(st_in_t[0].magma_repr()), 'O', Out(st_out_t.magma_repr())]
        if has_valid:
            IO += valid_ports

        @classmethod
        def definition(cls):
            op_inst = op()
            wire(cls.I, op_inst.I)
            wire(cls.O, op_inst.O)

            if has_valid:
                wire(cls.valid_up, op_inst.valid_up)
                wire(cls.valid_down, op_inst.valid_down)

    return _Add_1_0_T

@cache_definition
def DefineRemove_1_0_T(op: DefineCircuitKind, has_valid=False) -> DefineCircuitKind:
    class _Remove_1_0_T(Circuit):
        name = "Remove_1_S_op{}".format( cleanName(str(op)))
        binary_op = False
        st_in_t = [ST_TSeq(1, 0, op.st_in_t[0])]
        st_out_t = op.st_out_t
        IO = ['I', In(st_in_t.magma_repr()), 'O', Out(st_out_t.magma_repr())]
        if has_valid:
            IO += valid_ports

        @classmethod
        def definition(cls):
            op_inst = op()
            wire(cls.I, op_inst.I)
            wire(cls.O, op_inst.O)

            if has_valid:
                wire(cls.valid_up, op_inst.valid_up)
                wire(cls.valid_down, op_inst.valid_down)

    return _Remove_1_0_T
