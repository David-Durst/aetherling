from aetherling.space_time.type_helpers import *
from aetherling.space_time.nested_counters import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run

def test_nested_counters_TSeq_3_1():
    n = 3
    i = 1
    t = ST_TSeq(n, i, ST_Int())
    testcircuit = DefineNestedCounters(t)
    tester = fault.Tester(testcircuit, testcircuit.CLK)

    for j in range(n+i):
        tester.print("valid: %d\n", tester.circuit.valid)
        tester.print("last: %d\n", tester.circuit.last)
        tester.print("outer counter: %d\n", tester.circuit.Counter2_Mod4CE_inst0.O)
        tester.circuit.valid.expect(j < n)
        tester.circuit.last.expect(j == n+i - 1)
        tester.step(2)
    compile_and_run(tester)
