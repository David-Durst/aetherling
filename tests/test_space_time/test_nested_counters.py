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

    tester.eval()
    for j in range(n+i):
        tester.print("valid: %d\n", tester.circuit.valid)
        tester.print("last: %d\n", tester.circuit.last)
        tester.circuit.valid.expect(j < n)
        tester.circuit.last.expect(j == n+i - 1)
        tester.step(2)
    compile_and_run(tester)

def test_nested_counters_TSeq_3_1_SSeq_2():
    no = 3
    io = 1
    ni = 2
    t = ST_TSeq(no, io, ST_SSeq(ni, ST_Int()))
    testcircuit = DefineNestedCounters(t)
    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.eval()
    for j in range(no+io):
        tester.print("valid: %d\n", tester.circuit.valid)
        tester.print("last: %d\n", tester.circuit.last)
        tester.circuit.valid.expect(j < no)
        tester.circuit.last.expect(j == no+io - 1)
        tester.step(2)
    compile_and_run(tester)

def test_nested_counters_TSeq_3_1_TSeq_2_2():
    no = 3
    io = 1
    ni = 2
    ii = 2
    t = ST_TSeq(no, io, ST_TSeq(ni, ii, ST_Int()))
    testcircuit = DefineNestedCounters(t)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    num_clocks = (no+io) * (ni+ii)
    valid_clocks = [(o < no) and (i < ni) for o in range(no+io) for i in range(ni+ii)]
    tester.eval()
    for j in range((no+io) * (ni+ii)):
        tester.print("valid: %d\n", tester.circuit.valid)
        tester.print("last: %d\n", tester.circuit.last)
        tester.circuit.valid.expect(valid_clocks[j])
        tester.circuit.last.expect(j == num_clocks - 1)
        tester.step(2)
    compile_and_run(tester)

def test_nested_counters_TSeq_4_4_TSeq_1_1():
    no = 4
    io = 4
    ni = 1
    ii = 1
    t = ST_TSeq(no, io, ST_TSeq(ni, ii, ST_Int()))
    testcircuit = DefineNestedCounters(t)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    num_clocks = (no+io) * (ni+ii)
    valid_clocks = [(o < no) and (i < ni) for o in range(no+io) for i in range(ni+ii)]
    tester.eval()
    for j in range((no+io) * (ni+ii)):
        tester.print("valid: %d\n", tester.circuit.valid)
        tester.print("last: %d\n", tester.circuit.last)
        tester.circuit.valid.expect(valid_clocks[j])
        tester.circuit.last.expect(j == num_clocks - 1)
        tester.step(2)
    compile_and_run(tester)
