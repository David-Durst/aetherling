from aetherling.space_time import *
from magma import *
from magma.clock import *
from magma.bitutils import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run, wrap_module_with_top

def test_mul_pipelined():
    n = 4
    i = 0
    delay = 2
    test_vals = [[2,4,6,8],[1,3,5,9]]
    red = DefineMap_T(n, i, DefineMul_Atom(has_valid=True))

    tester = fault.Tester(red, red.CLK)

    tester.circuit.valid_up = 1
    for i in range(n + delay):
        tester.print("clk: {}\n".format(i))
        if i < n:
            tester.circuit.I[0] = test_vals[0][i]
            tester.circuit.I[1] = test_vals[1][i]
        tester.eval()
        if i > delay:
            tester.circuit.valid_down.expect(1)
            tester.circuit.O.expect(test_vals[0][i - 2] * test_vals[1][i - 2])
        tester.step(2)
    compile_and_run(tester)
