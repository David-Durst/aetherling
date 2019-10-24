from aetherling.space_time import *
from magma import *
from magma.clock import *
from magma.bitutils import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run, wrap_module_with_top
from aetherling.space_time.modules.serialize import DefineSerialize

def test_serialize_basic():
    n = 5
    i = 0
    inputs = list(range(n))
    num_iterations = 2
    t = ST_Int()


    ser = DefineSerialize(n, i, t)

    tester = fault.Tester(ser, ser.CLK)

    tester.circuit.valid_up = 1
    tester.circuit.I = inputs
    for clk in range(num_iterations):
        tester.print("clk: {}\n".format(clk))
        tester.eval()
        tester.circuit.valid_down.expect(1)
        #tester.circuit.O[0].expect(inputs[clk % n])
        tester.step(2)
    compile_and_run(tester)
