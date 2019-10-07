import fault
import aetherling.helpers.fault_helpers as fault_helpers
from aetherling.space_time import *
from aetherling.space_time.reshape_st import DefineReshape_ST

@cache_definition 
def Module_0() -> DefineCircuitKind:
    class _Module_0(Circuit):
        name = "Module_0"
        IO = ['tuple_in_1', In(ST_Int().magma_repr()), 'tuple_in_2', In(ST_Int().magma_repr()), 'O', Out(ST_SSeq_Tuple(2, ST_Int()).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_Int(), ST_Int()]
        st_out_t = ST_SSeq_Tuple(2, ST_Int())
        binary_op = True
        @classmethod
        def definition(cls):
            n8 = DefineSSeqTupleCreator(ST_Int(), has_valid=True)()
            wire(cls.tuple_in_1, n8.I0)
            wire(cls.tuple_in_2, n8.I1)
            wire(cls.valid_up & cls.valid_up, n8.valid_up)
            wire(n8.O, cls.O)
            wire(n8.valid_down, cls.valid_down)
    return _Module_0

@cache_definition 
def Module_1() -> DefineCircuitKind:
    class _Module_1(Circuit):
        name = "Module_1"
        IO = ['f_in1', In(ST_SSeq(50, ST_Int()).magma_repr()), 'f_in2', In(ST_SSeq(50, ST_Int()).magma_repr()), 'O', Out(ST_SSeq(50, ST_SSeq_Tuple(2, ST_Int())).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_SSeq(50, ST_Int()), ST_SSeq(50, ST_Int())]
        st_out_t = ST_SSeq(50, ST_SSeq_Tuple(2, ST_Int()))
        binary_op = True
        @classmethod
        def definition(cls):
            n11 = DefineMap2_S(50, Module_0(),True)()
            wire(cls.f_in1, n11.tuple_in_1)
            wire(cls.f_in2, n11.tuple_in_2)
            wire(cls.valid_up & cls.valid_up, n11.valid_up)
            wire(n11.O, cls.O)
            wire(n11.valid_down, cls.valid_down)
    return _Module_1

@cache_definition 
def Module_2() -> DefineCircuitKind:
    class _Module_2(Circuit):
        name = "Module_2"
        IO = ['tuple_in_1', In(ST_SSeq_Tuple(2, ST_Int()).magma_repr()), 'tuple_in_2', In(ST_Int().magma_repr()), 'O', Out(ST_SSeq_Tuple(3, ST_Int()).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_SSeq_Tuple(2, ST_Int()), ST_Int()]
        st_out_t = ST_SSeq_Tuple(3, ST_Int())
        binary_op = True
        @classmethod
        def definition(cls):
            n2 = DefineSSeqTupleAppender(ST_Int(), 2, has_valid=True)()
            wire(cls.tuple_in_1, n2.I0)
            wire(cls.tuple_in_2, n2.I1)
            wire(cls.valid_up & cls.valid_up, n2.valid_up)
            wire(n2.O, cls.O)
            wire(n2.valid_down, cls.valid_down)
    return _Module_2

@cache_definition 
def Module_3() -> DefineCircuitKind:
    class _Module_3(Circuit):
        name = "Module_3"
        IO = ['f_in1', In(ST_SSeq(50, ST_SSeq_Tuple(2, ST_Int())).magma_repr()), 'f_in2', In(ST_SSeq(50, ST_Int()).magma_repr()), 'O', Out(ST_SSeq(50, ST_SSeq_Tuple(3, ST_Int())).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_SSeq(50, ST_SSeq_Tuple(2, ST_Int())), ST_SSeq(50, ST_Int())]
        st_out_t = ST_SSeq(50, ST_SSeq_Tuple(3, ST_Int()))
        binary_op = True
        @classmethod
        def definition(cls):
            n5 = DefineMap2_S(50, Module_2(),True)()
            wire(cls.f_in1, n5.tuple_in_1)
            wire(cls.f_in2, n5.tuple_in_2)
            wire(cls.valid_up & cls.valid_up, n5.valid_up)
            wire(n5.O, cls.O)
            wire(n5.valid_down, cls.valid_down)
    return _Module_3

@cache_definition 
def Module_4() -> DefineCircuitKind:
    class _Module_4(Circuit):
        name = "Module_4"
        IO = ['hi', In(ST_TSeq(2, 0, ST_SSeq(50, ST_Int())).magma_repr()), 'O', Out(ST_TSeq(2, 0, ST_SSeq(50, ST_SSeq(3, ST_Int()))).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = ST_TSeq(2, 0, ST_SSeq(50, ST_Int()))
        st_out_t = ST_TSeq(2, 0, ST_SSeq(50, ST_SSeq(3, ST_Int())))
        binary_op = False
        @classmethod
        def definition(cls):
            n13 = DefineShift_TS(2, 0, 50, 1, ST_Int(), has_valid=True)()
            wire(cls.hi, n13.I)
            wire(cls.valid_up, n13.valid_up)
            n14 = DefineShift_TS(2, 0, 50, 1, ST_Int(), has_valid=True)()
            wire(n13.O, n14.I)
            wire(n13.valid_down, n14.valid_up)
            n15 = DefineMap2_T(2, 0, Module_1())()
            wire(n14.O, n15.f_in1)
            wire(n13.O, n15.f_in2)
            wire(n14.valid_down & n13.valid_down, n15.valid_up)
            n16 = DefineMap2_T(2, 0, Module_3())()
            wire(n15.O, n16.f_in1)
            wire(cls.hi, n16.f_in2)
            wire(n15.valid_down & cls.valid_up, n16.valid_up)
            n17 = DefineReshape_ST(ST_TSeq(2, 0, ST_SSeq(50, ST_SSeq_Tuple(3, ST_Int()))), ST_TSeq(2, 0, ST_SSeq(50, ST_SSeq(3, ST_Int()))), has_valid=True)()
            wire(n16.O, n17.I)
            wire(n16.valid_down, n17.valid_up)
            wire(n17.O, cls.O)
            wire(n17.valid_down, cls.valid_down)
    return _Module_4

Main = Module_4
fault_inputs0 = [[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50],[51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]]
fault_inputs0_valid = [True,True]
fault_output = [[-23451,-23451,1,-23451,1,2,1,2,3,2,3,4,3,4,5,4,5,6,5,6,7,6,7,8,7,8,9,8,9,10,9,10,11,10,11,12,11,12,13,12,13,14,13,14,15,14,15,16,15,16,17,16,17,18,17,18,19,18,19,20,19,20,21,20,21,22,21,22,23,22,23,24,23,24,25,24,25,26,25,26,27,26,27,28,27,28,29,28,29,30,29,30,31,30,31,32,31,32,33,32,33,34,33,34,35,34,35,36,35,36,37,36,37,38,37,38,39,38,39,40,39,40,41,40,41,42,41,42,43,42,43,44,43,44,45,44,45,46,45,46,47,46,47,48,47,48,49,48,49,50],[49,50,51,50,51,52,51,52,53,52,53,54,53,54,55,54,55,56,55,56,57,56,57,58,57,58,59,58,59,60,59,60,61,60,61,62,61,62,63,62,63,64,63,64,65,64,65,66,65,66,67,66,67,68,67,68,69,68,69,70,69,70,71,70,71,72,71,72,73,72,73,74,73,74,75,74,75,76,75,76,77,76,77,78,77,78,79,78,79,80,79,80,81,80,81,82,81,82,83,82,83,84,83,84,85,84,85,86,85,86,87,86,87,88,87,88,89,88,89,90,89,90,91,90,91,92,91,92,93,92,93,94,93,94,95,94,95,96,95,96,97,96,97,98,97,98,99,98,99,100]]
fault_output_valid = [True,True]
if __name__ == '__main__':
    mod = Main()
    tester = fault.Tester(mod, mod.CLK)
    tester.circuit.valid_up = 1
    output_counter = 0
    for f_clk in range(2 + 1):
        tester.print('clk: {}\n'.format(f_clk))
        if f_clk < 2 and fault_inputs0_valid[f_clk]:
            fault_helpers.set_nested_port(tester, tester.circuit.hi, fault_inputs0[f_clk], num_nested_space_layers(ST_TSeq(2, 0, ST_SSeq(50, ST_Int()))), 0)
            tester.print("hi: ")
            fault_helpers.print_nested_port(tester, tester.circuit.hi, num_nested_space_layers(ST_TSeq(2, 0, ST_SSeq(50, ST_Int()))))
            tester.print("\n")
        tester.eval()
        if f_clk > 1:
            output_counter += 1
        if f_clk >= 1 and fault_output_valid[output_counter]:
            tester.print("O: ")
            fault_helpers.print_nested_port(tester, tester.circuit.O, num_nested_space_layers(ST_TSeq(2, 0, ST_SSeq(50, ST_SSeq(3, ST_Int())))))
            tester.print("\n")
            fault_helpers.expect_nested_port(tester, tester.circuit.O, fault_output[output_counter], num_nested_space_layers(ST_TSeq(2, 0, ST_SSeq(50, ST_SSeq(3, ST_Int())))), 0)
        tester.step(2)
    fault_helpers.compile_and_run(tester)
