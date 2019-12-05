from aetherling.space_time import *
from magma import *
from magma.clock import *
from magma.bitutils import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run

def test_down_s():
    num_out = 1
    test_val = 3

    down = DefineDown_S(1, 0, ST_Int(), has_valid=True)

    tester = fault.Tester(down, down.CLK)
    tester.circuit.CLK = 1

    tester.circuit.valid_up = 1
    tester.circuit.I[0] = test_val
    tester.eval()
    for i in range(num_out):
        tester.circuit.O[i].expect(test_val)
    tester.circuit.valid_down.expect(1)
    compile_and_run(tester)

def test_down_t():
    num_in = 3
    test_val = 3

    down = DefineDown_T(3, 0, 1, ST_Int(), has_valid=True)

    tester = fault.Tester(down, down.CLK)
    tester.circuit.CLK = 1

    tester.circuit.valid_up = 1
    tester.circuit.I = test_val
    tester.eval()
    for i in range(num_in):
        tester.print("clk: {}\n".format(i))
        # once start valid always valid
        tester.circuit.valid_down.expect(i >= 1)
        if i == 1:
            tester.circuit.O.expect(test_val)
        tester.step(2)
    tester.circuit.valid_down.expect(1)
    compile_and_run(tester)
