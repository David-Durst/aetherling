import fault
import aetherling.helpers.fault_helpers as fault_helpers
from aetherling.space_time import *
from aetherling.space_time.reshape_st import DefineReshape_ST
import magma as m
Main = lambda : m.DefineFromVerilog("/home/david/dev/embeddedHaskellAetherling/test/verilog_examples/aetherling_copies/map_s_4.v", target_modules=["top"])[0]
fault_inputs0 = [0,-1,2,3]
fault_inputs0_valid = [True,True,True,True]
fault_output = [0,1,2,3]
fault_output_valid = [True,True,True,True]
if __name__ == '__main__':
    mod = Main()
    tester = fault.Tester(mod, mod.CLK)
    tester.circuit.valid_up = 1
    output_counter = 0
    for f_clk in range(4 + 0):
        tester.print('clk: {}\n'.format(f_clk))
        if f_clk < 4 and fault_inputs0_valid[f_clk]:
            fault_helpers.set_nested_port(tester, tester.circuit.hi, fault_inputs0[f_clk], num_nested_space_layers(ST_TSeq(4, 0, ST_Int())), 0)
            tester.print("hi: ")
            fault_helpers.print_nested_port(tester, tester.circuit.hi, num_nested_space_layers(ST_TSeq(4, 0, ST_Int())))
            tester.print("\n")
        tester.eval()
        if f_clk > 0:
            output_counter += 1
        if f_clk >= 0 and fault_output_valid[output_counter]:
            tester.print("O: ")
            fault_helpers.print_nested_port(tester, tester.circuit.O, num_nested_space_layers(ST_TSeq(4, 0, ST_Int())))
            tester.print("\n")
            fault_helpers.expect_nested_port(tester, tester.circuit.O, fault_output[output_counter], num_nested_space_layers(ST_TSeq(4, 0, ST_Int())), 0)
        tester.step(2)
    fault_helpers.compile_and_run(tester)
