import fault
import aetherling.helpers.fault_helpers as fault_helpers
from aetherling.space_time import *
from aetherling.space_time.reshape_st import DefineReshape_ST
import magma as m

@cache_definition 
def Module_0() -> DefineCircuitKind:
    class _Module_0(Circuit):
        name = "Module_0"
        IO = ['I', In(ST_Int().magma_repr()), 'O', Out(ST_Bit().magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_Int()]
        st_out_t = ST_Bit()
        binary_op = False
        @classmethod
        def definition(cls):
            n3 = DefineConst(ST_Int(), (2,), has_valid=True, delay=1)()
            wire(cls.valid_up, n3.valid_up)
            n5 = DefineAtomTupleCreator(ST_Int(), ST_Int(), has_valid=True)()
            wire(cls.I, n5.I0)
            wire(n3.O, n5.I1)
            wire(cls.valid_up & n3.valid_down, n5.valid_up)
            n6 = DefineLt_Atom(True)()
            wire(n5.O, n6.I)
            wire(n5.valid_down, n6.valid_up)
            wire(n6.O, cls.O)
            wire(n6.valid_down, cls.valid_down)
    return _Module_0

@cache_definition 
def Module_1() -> DefineCircuitKind:
    class _Module_1(Circuit):
        name = "top"
        IO = ['I', In(ST_SSeq(4, ST_Int()).magma_repr()), 'O', Out(ST_SSeq(4, ST_Bit()).magma_repr())] + ClockInterface(has_ce=False,has_reset=False) + valid_ports
        st_in_t = [ST_SSeq(4, ST_Int())]
        st_out_t = ST_SSeq(4, ST_Bit())
        binary_op = False
        @classmethod
        def definition(cls):
            n1 = DefineFIFO(ST_SSeq(4, ST_Int()), 1, has_valid=True)()
            wire(cls.I, n1.I)
            wire(cls.valid_up, n1.valid_up)
            n7 = DefineMap_S(4, Module_0(),True)()
            wire(n1.O, n7.I)
            wire(n1.valid_down, n7.valid_up)
            n8 = DefineFIFO(ST_SSeq(4, ST_Bit()), 1, has_valid=True)()
            wire(n7.O, n8.I)
            wire(n7.valid_down, n8.valid_up)
            n9 = DefineFIFO(ST_SSeq(4, ST_Bit()), 1, has_valid=True)()
            wire(n8.O, n9.I)
            wire(n8.valid_down, n9.valid_up)
            n10 = DefineFIFO(ST_SSeq(4, ST_Bit()), 1, has_valid=True)()
            wire(n9.O, n10.I)
            wire(n9.valid_down, n10.valid_up)
            wire(n10.O, cls.O)
            wire(n10.valid_down, cls.valid_down)
    return _Module_1

Main = Module_1
fault_inputs0 = [[0,1,2,3]]
fault_inputs0_valid = [True]
fault_output = [[1,0,0,0]]
fault_output_valid = [True]
if __name__ == '__main__':
    mod = Main()
    tester = fault.Tester(mod, clock(mod.CLK))
    tester.circuit.valid_up = 1
    output_counter = 0
    for f_clk in range(1 + 4):
        tester.print('clk: {}\n'.format(f_clk))
        if f_clk < 1 and fault_inputs0_valid[f_clk]:
            fault_helpers.set_nested_port(tester, tester.circuit.I, fault_inputs0[f_clk], num_nested_space_layers(ST_SSeq(4, ST_Int())), 0)
            tester.print("I: ")
            fault_helpers.print_nested_port(tester, tester.circuit.I, num_nested_space_layers(ST_SSeq(4, ST_Int())))
            tester.print("\n")
        tester.eval()
        if f_clk > 4:
            output_counter += 1
        tester.print("O: ")
        fault_helpers.print_nested_port(tester, tester.circuit.O, num_nested_space_layers(ST_SSeq(4, ST_Bit())))
        tester.print("\n")
        if f_clk >= 4:
            tester.circuit.valid_down.expect(1)
        if f_clk >= 4 and fault_output_valid[output_counter]:
            fault_helpers.expect_nested_port(tester, tester.circuit.O, fault_output[output_counter], num_nested_space_layers(ST_SSeq(4, ST_Bit())), 0)
        tester.step(2)
    fault_helpers.compile_and_run(tester)
