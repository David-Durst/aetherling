import fault
import aetherling.helpers.fault_helpers as fault_helpers
from aetherling.space_time import *
from aetherling.space_time.reshape_st import DefineReshape_ST

@cache_definition 
def Module_0() -> DefineCircuitKind:
    class _Module_0(Circuit):
        name = "Module_0"
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
    return _Module_0

@cache_definition 
def Module_1() -> DefineCircuitKind:
    class _Module_1(Circuit):
        name = "Module_1"
        IO = ['hi', In(ST_SSeq(4, ST_TSeq(4, 0, ST_Int())).magma_repr()), 'O', Out(ST_TSeq(1, 3, ST_SSeq(1, ST_Int())).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = ST_SSeq(4, ST_TSeq(4, 0, ST_Int()))
        st_out_t = ST_TSeq(1, 3, ST_SSeq(1, ST_Int()))
        binary_op = False
        @classmethod
        def definition(cls):
            n3 = DefineDown_S(4, 0, ST_TSeq(4, 0, ST_Int()), has_valid=True)()
            wire(cls.hi, n3.I)
            wire(cls.valid_up, n3.valid_up)
            n4 = DefineReshape_ST(ST_SSeq(1, ST_TSeq(4, 0, ST_Int())), ST_TSeq(4, 0, ST_SSeq(1, ST_Int())), has_valid=True)()
            wire(n3.O, n4.I)
            wire(n3.valid_down, n4.valid_up)
            n6 = DefineMap_T(1, 3, Module_0())()
            wire(n4.O, n6.f_in)
            wire(n4.valid_down, n6.valid_up)
            wire(n6.O, cls.O)
            wire(n6.valid_down, cls.valid_down)
    return _Module_1

Main = Module_1
fault_inputs0 = [[1,5,9,13],[2,6,10,14],[3,7,11,15],[4,8,12,16]]
fault_inputs0_valid = [True,True,True,True]
fault_output = [1,0,0,0]
fault_output_valid = [True,False,False,False]
if __name__ == '__main__':
    mod = Main()
    tester = fault.Tester(mod, mod.CLK)
    tester.circuit.valid_up = 1
    output_counter = 0
    for f_clk in range(4 + 1):
        tester.print('clk: {}\n'.format(f_clk))
        if f_clk < 4 and fault_inputs0_valid[f_clk]:
            fault_helpers.set_nested_port(tester, tester.circuit.hi, fault_inputs0[f_clk], num_nested_space_layers(ST_SSeq(4, ST_TSeq(4, 0, ST_Int()))), 0)
        tester.eval()
        if f_clk > 1:
            output_counter += 1
        if f_clk >= 1 and fault_output_valid[output_counter]:
            fault_helpers.expect_nested_port(tester, tester.circuit.O, fault_output[output_counter], num_nested_space_layers(ST_TSeq(1, 3, ST_SSeq(1, ST_Int()))), 0)
        tester.step(2)
    fault_helpers.compile_and_run(tester)
