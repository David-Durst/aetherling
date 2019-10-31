import fault
import aetherling.helpers.fault_helpers as fault_helpers
from aetherling.space_time import *
from aetherling.space_time.reshape_st import DefineReshape_ST
import magma as m

@cache_definition 
def Module_0() -> DefineCircuitKind:
    class _Module_0(Circuit):
        name = "Module_0"
        IO = ['I', In(ST_Int().magma_repr()), 'O', Out(ST_Int().magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_Int()]
        st_out_t = ST_Int()
        binary_op = False
        @classmethod
        def definition(cls):
            n3 = DefineConst(ST_Int(), (5,), has_valid=True, delay=1)()
            wire(cls.valid_up, n3.valid_up)
            n5 = DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True)()
            wire(cls.I, n5.I0)
            wire(n3.O, n5.I1)
            wire(cls.valid_up & n3.valid_down, n5.valid_up)
            n6 = DefineAdd_Atom(True)()
            wire(n5.O, n6.I)
            wire(n5.valid_down, n6.valid_up)
            wire(n6.O, cls.O)
            wire(n6.valid_down, cls.valid_down)
    return _Module_0

@cache_definition 
def Module_1() -> DefineCircuitKind:
    class _Module_1(Circuit):
        name = "top"
        IO = ['I', In(ST_SSeq(200, ST_Int()).magma_repr()), 'O', Out(ST_SSeq(200, ST_Int()).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_SSeq(200, ST_Int())]
        st_out_t = ST_SSeq(200, ST_Int())
        binary_op = False
        @classmethod
        def definition(cls):
            n1 = DefineFIFO(ST_SSeq(200, ST_Int()), 1, has_valid=True)()
            wire(cls.I, n1.I)
            wire(cls.valid_up, n1.valid_up)
            n7 = DefineMap_S(200, Module_0(),True)()
            wire(n1.O, n7.I)
            wire(n1.valid_down, n7.valid_up)
            n8 = DefineFIFO(ST_SSeq(200, ST_Int()), 1, has_valid=True)()
            wire(n7.O, n8.I)
            wire(n7.valid_down, n8.valid_up)
            n9 = DefineFIFO(ST_SSeq(200, ST_Int()), 1, has_valid=True)()
            wire(n8.O, n9.I)
            wire(n8.valid_down, n9.valid_up)
            wire(n9.O, cls.O)
            wire(n9.valid_down, cls.valid_down)
    return _Module_1

Main = Module_1
fault_inputs0 = [[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200]]
fault_inputs0_valid = [True]
fault_output = [[6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205]]
fault_output_valid = [True]
if __name__ == '__main__':
    mod = Main()
    tester = fault.Tester(mod, clock(mod.CLK))
    tester.circuit.valid_up = 1
    output_counter = 0
    for f_clk in range(1 + 3):
        tester.print('clk: {}\n'.format(f_clk))
        if f_clk < 1 and fault_inputs0_valid[f_clk]:
            fault_helpers.set_nested_port(tester, tester.circuit.I, fault_inputs0[f_clk], num_nested_space_layers(ST_SSeq(200, ST_Int())), 0)
            tester.print("I: ")
            fault_helpers.print_nested_port(tester, tester.circuit.I, num_nested_space_layers(ST_SSeq(200, ST_Int())))
            tester.print("\n")
        tester.eval()
        if f_clk > 3:
            output_counter += 1
        tester.print("O: ")
        fault_helpers.print_nested_port(tester, tester.circuit.O, num_nested_space_layers(ST_SSeq(200, ST_Int())))
        tester.print("\n")
        if f_clk >= 3:
            tester.circuit.valid_down.expect(1)
        if f_clk >= 3 and fault_output_valid[output_counter]:
            fault_helpers.expect_nested_port(tester, tester.circuit.O, fault_output[output_counter], num_nested_space_layers(ST_SSeq(200, ST_Int())), 0)
        tester.step(2)
    fault_helpers.compile_and_run(tester)
