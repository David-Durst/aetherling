import fault
import aetherling.helpers.fault_helpers as fault_helpers
from aetherling.space_time import *
from aetherling.space_time.reshape_st import DefineReshape_ST

@cache_definition 
def Module_0() -> DefineCircuitKind:
    class _Module_0(Circuit):
        name = "Module_0"
        IO = ['f_in', In(ST_SSeq(2, ST_TSeq(4, 0, ST_Int())).magma_repr()), 'O', Out(ST_SSeq(1, ST_TSeq(4, 0, ST_Int())).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = ST_SSeq(2, ST_TSeq(4, 0, ST_Int()))
        st_out_t = ST_SSeq(1, ST_TSeq(4, 0, ST_Int()))
        binary_op = False
        @classmethod
        def definition(cls):
            n3 = DefineDown_S(2, 0, ST_TSeq(4, 0, ST_Int()), has_valid=True)()
            wire(cls.f_in, n3.I)
            wire(cls.valid_up, n3.valid_up)
            wire(n3.O, cls.O)
            wire(n3.valid_down, cls.valid_down)
    return _Module_0

@cache_definition 
def Module_1() -> DefineCircuitKind:
    class _Module_1(Circuit):
        name = "Module_1"
        IO = ['f_in', In(ST_SSeq(1, ST_Int()).magma_repr()), 'O', Out(ST_SSeq(1, ST_Int()).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = ST_SSeq(1, ST_Int())
        st_out_t = ST_SSeq(1, ST_Int())
        binary_op = False
        @classmethod
        def definition(cls):
            n1 = DefineDown_S(1, 0, ST_Int(), has_valid=True)()
            wire(cls.f_in, n1.I)
            wire(cls.valid_up, n1.valid_up)
            wire(n1.O, cls.O)
            wire(n1.valid_down, cls.valid_down)
    return _Module_1

@cache_definition 
def Module_2() -> DefineCircuitKind:
    class _Module_2(Circuit):
        name = "Module_2"
        IO = ['hi', In(ST_TSeq(2, 0, ST_SSeq(2, ST_TSeq(4, 0, ST_Int()))).magma_repr()), 'O', Out(ST_TSeq(1, 7, ST_SSeq(1, ST_Int())).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = ST_TSeq(2, 0, ST_SSeq(2, ST_TSeq(4, 0, ST_Int())))
        st_out_t = ST_TSeq(1, 7, ST_SSeq(1, ST_Int()))
        binary_op = False
        @classmethod
        def definition(cls):
            n5 = DefineDown_T(2, 0, 0, ST_SSeq(2, ST_TSeq(4, 0, ST_Int())), has_valid=True)()
            wire(cls.hi, n5.I)
            wire(cls.valid_up, n5.valid_up)
            n6 = DefineMap_T(1, 1, Module_0())()
            wire(n5.O, n6.f_in)
            wire(n5.valid_down, n6.valid_up)
            n7 = DefineReshape_ST(ST_TSeq(1, 1, ST_SSeq(1, ST_TSeq(4, 0, ST_Int()))), ST_TSeq(4, 4, ST_SSeq(1, ST_Int())), has_valid=True)()
            wire(n6.O, n7.I)
            wire(n6.valid_down, n7.valid_up)
            n8 = DefineDown_T(4, 4, 0, ST_SSeq(1, ST_Int()), has_valid=True)()
            wire(n7.O, n8.I)
            wire(n7.valid_down, n8.valid_up)
            n9 = DefineMap_T(1, 7, Module_1())()
            wire(n8.O, n9.f_in)
            wire(n8.valid_down, n9.valid_up)
            wire(n9.O, cls.O)
            wire(n9.valid_down, cls.valid_down)
    return _Module_2

Main = Module_2
fault_inputs0 = [[1,5],[2,6],[3,7],[4,8],[9,13],[10,14],[11,15],[12,16]]
fault_inputs0_valid = [True,True,True,True,True,True,True,True]
fault_output = [1,0,0,0,0,0,0,0]
fault_output_valid = [True,False,False,False,False,False,False,False]
if __name__ == '__main__':
    mod = Main()
    tester = fault.Tester(mod, mod.CLK)
    tester.circuit.valid_up = 1
    output_counter = 0
    for f_clk in range(8 + 1):
        tester.print('clk: {}\n'.format(f_clk))
        if f_clk < 8 and fault_inputs0_valid[f_clk]:
            fault_helpers.set_nested_port(tester, tester.circuit.hi, fault_inputs0[f_clk], num_nested_space_layers(ST_TSeq(2, 0, ST_SSeq(2, ST_TSeq(4, 0, ST_Int())))), 0)
        tester.eval()
        if f_clk > 1:
            output_counter += 1
        if f_clk >= 1 and fault_output_valid[output_counter]:
            fault_helpers.expect_nested_port(tester, tester.circuit.O, fault_output[output_counter], num_nested_space_layers(ST_TSeq(1, 7, ST_SSeq(1, ST_Int()))), 0)
        tester.step(2)
    fault_helpers.compile_and_run(tester)
