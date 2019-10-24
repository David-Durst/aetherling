from aetherling.modules.mux_any_type import DefineMuxAnyType
from magma import *
from magma.bitutils import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run
from aetherling.space_time.type_helpers import ST_Int
import builtins

def test_mux_int_5():
    n = 5
    T = ST_Int().magma_repr()

    testcircuit = DefineMuxAnyType(T, n)

    tester = fault.Tester(testcircuit)

    for i in range(n):
        tester.circuit.data[i] = i
    tester.circuit.sel = 2
    tester.eval()
    tester.circuit.out.expect(2)
    compile_and_run(tester)

