import fault
import aetherling.helpers.fault_helpers as fault_helpers
from aetherling.space_time import *
from aetherling.space_time.reshape_st import DefineReshape_ST

@cache_definition 
def Module_0() -> DefineCircuitKind:
    class _Module_0(Circuit):
        name = "Module_0"
        IO = ['f_in1', In(ST_Int().magma_repr()), 'f_in2', In(ST_Int().magma_repr()), 'O', Out(ST_Atom_Tuple(ST_Int(), ST_Int()).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_Int(), ST_Int()]
        st_out_t = ST_Int()
        binary_op = True
        @classmethod
        def definition(cls):
            n4 = DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True)()
            wire(cls.f_in1, n4.I0)
            wire(cls.f_in2, n4.I1)
            wire(cls.valid_up & cls.valid_up, n4.valid_up)
            wire(n4.O, cls.O)
            wire(n4.valid_down, cls.valid_down)
    return _Module_0

@cache_definition 
def Module_1() -> DefineCircuitKind:
    class _Module_1(Circuit):
        name = "Module_1"
        IO = ['f_in', In(ST_Atom_Tuple(ST_Int(), ST_Int()).magma_repr()), 'O', Out(ST_Int().magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = ST_Atom_Tuple(ST_Int(), ST_Int())
        st_out_t = ST_Int()
        binary_op = False
        @classmethod
        def definition(cls):
            n1 = DefineMul_Atom(True)()
            wire(cls.f_in, n1.I)
            wire(cls.valid_up, n1.valid_up)
            wire(n1.O, cls.O)
            wire(n1.valid_down, cls.valid_down)
    return _Module_1

@cache_definition 
def Module_2() -> DefineCircuitKind:
    class _Module_2(Circuit):
        name = "Module_2"
        IO = ['f_in', In(ST_Atom_Tuple(ST_Int(), ST_Int()).magma_repr()), 'O', Out(ST_Int().magma_repr())] + ClockInterface(has_ce=False,has_reset=False) 
        st_in_t = ST_Atom_Tuple(ST_Int(), ST_Int())
        st_out_t = ST_Int()
        binary_op = False
        @classmethod
        def definition(cls):
            n10 = DefineAdd_Atom(False)()
            wire(cls.f_in, n10.I)
            wire(n10.O, cls.O)
    return _Module_2

@cache_definition 
def Module_3() -> DefineCircuitKind:
    class _Module_3(Circuit):
        name = "Module_3"
        IO = ['outoutout', Out(ST_SSeq(8, ST_Atom_Tuple(ST_Int(), ST_Int())).magma_repr()), 'hi', In(ST_SSeq(8, ST_Int()).magma_repr()), 'O', Out(ST_SSeq(1, ST_Int()).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = ST_SSeq(8, ST_Int())
        st_out_t = ST_Int()
        binary_op = False
        @classmethod
        def definition(cls):
            n5 = DefineConst(ST_SSeq(8, ST_Int()), ((1,2,3,4,2,1,2,3,),),has_valid=True)()
            wire(cls.valid_up, n5.valid_up)
            n7 = DefineMap2_S(8, Module_0())()
            wire(n5.O, n7.f_in1)
            wire(cls.hi, n7.f_in2)
            wire(n5.valid_down & cls.valid_up, n7.valid_up)
            n8 = DefineMap_S(8, Module_1())()
            wire(n7.O, n8.f_in)
            wire(n7.O, cls.outoutout)
            wire(n7.valid_down, n8.valid_up)
            n11 = DefineReduce_S(8, Module_2(), has_valid=True)()
            wire(n8.O, n11.I)
            wire(n8.valid_down, n11.valid_up)
            wire(n11.O, cls.O)
            wire(n11.valid_down, cls.valid_down)
    return _Module_3

Main = Module_3
fault_inputs0 = [[10,8,9,3,4,2,2,2]]
fault_inputs0_valid = [True]
fault_output = [85]
fault_output_valid = [True]
if __name__ == '__main__':
    mod = Main()
    tester = fault.Tester(mod, mod.CLK)
    tester.circuit.valid_up = 1
    output_counter = 0
    for f_clk in range(1 + 0):
        tester.print('clk: {}\n'.format(f_clk))
        if f_clk < 1 and fault_inputs0_valid[f_clk]:
            fault_helpers.set_nested_port(tester, tester.circuit.hi, fault_inputs0[f_clk], num_nested_space_layers(ST_SSeq(8, ST_Int())), 0)
        tester.eval()
        if f_clk > 0:
            output_counter += 1
        if f_clk >= 0 and fault_output_valid[output_counter]:
            #for i in range(8):
            #    tester.print("output {}: %d\n".format(i), mod.instances[0].O[0])
            fault_helpers.expect_nested_port(tester, tester.circuit.O, fault_output[output_counter], num_nested_space_layers(ST_SSeq(1, ST_Int())), 0)
        tester.step(2)
    fault_helpers.compile_and_run(tester)
