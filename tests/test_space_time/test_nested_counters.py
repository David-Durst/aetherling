from aetherling.space_time.type_helpers import *
from aetherling.space_time.nested_counters import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run

def test_nested_counters_TSeq_2_0():
    n = 2
    i = 0
    t = ST_TSeq(n, i, ST_Int())
    testcircuit = DefineNestedCounters(t)
    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.eval()
    for j in range(n+i):
        tester.print("clk: {}\n".format(str(j)))
        tester.print("valid: %d\n", tester.circuit.valid)
        tester.print("last: %d\n", tester.circuit.last)
        tester.circuit.valid.expect(j < n)
        tester.circuit.last.expect(j == n+i - 1)
        tester.step(2)
    compile_and_run(tester)

def test_nested_counters_TSeq_2_0_no_last():
    n = 2
    i = 0
    t = ST_TSeq(n, i, ST_Int())
    testcircuit = DefineNestedCounters(t, has_last=False)
    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.eval()
    for j in range(n+i):
        tester.print("clk: {}\n".format(str(j)))
        tester.print("valid: %d\n", tester.circuit.valid)
        tester.circuit.valid.expect(j < n)
        tester.step(2)
    compile_and_run(tester)

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

def test_nested_counters_TSeq_3_1_ce_valid():
    n = 3
    i = 1
    t = ST_TSeq(n, i, ST_Int())
    testcircuit = DefineNestedCounters(t, has_cur_valid=True, has_ce=True)
    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.circuit.CE = 1
    tester.eval()
    for k in range(2):
        cur_valid = 0
        for j in range(n+i):
            tester.print("valid: %d\n", tester.circuit.valid)
            tester.print("last: %d\n", tester.circuit.last)
            tester.circuit.valid.expect(j < n)
            tester.circuit.last.expect(j == n+i - 1)
            if j < n:
                tester.circuit.cur_valid.expect(cur_valid)
            tester.step(2)
            cur_valid += 1 if j < n else 0
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

def test_nested_counters_TSeq_3_1_SSeq_2_ce_valid():
    no = 3
    io = 1
    ni = 2
    t = ST_TSeq(no, io, ST_SSeq(ni, ST_Int()))
    testcircuit = DefineNestedCounters(t, has_cur_valid=True, has_ce=True)
    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.circuit.CE = 1
    tester.eval()
    for k in range(2):
        cur_valid = 0
        for j in range(no+io):
            tester.print("valid: %d\n", tester.circuit.valid)
            tester.print("last: %d\n", tester.circuit.last)
            tester.circuit.valid.expect(j < no)
            tester.circuit.last.expect(j == no+io - 1)
            if j < no:
                tester.circuit.cur_valid.expect(cur_valid)
            tester.step(2)
            cur_valid += 1 if j < no else 0
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

def test_nested_counters_TSeq_3_1_TSeq_2_2_ce_valid():
    no = 3
    io = 1
    ni = 2
    ii = 2
    t = ST_TSeq(no, io, ST_TSeq(ni, ii, ST_Int()))
    testcircuit = DefineNestedCounters(t, has_cur_valid=True, has_ce=True)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    num_clocks = (no+io) * (ni+ii)
    valid_clocks = [(o < no) and (i < ni) for o in range(no+io) for i in range(ni+ii)]

    tester.circuit.CE = 1
    tester.eval()
    cur_clk = 0
    for k in range(2):
        cur_valid = 0
        for j in range((no+io) * (ni+ii)):
            tester.print("clk: {} \n".format(cur_clk))
            cur_clk += 1
            tester.print("valid: %d\n", tester.circuit.valid)
            tester.print("last: %d\n", tester.circuit.last)
            tester.circuit.valid.expect(valid_clocks[j])
            tester.circuit.last.expect(j == num_clocks - 1)
            if valid_clocks[j]:
                tester.circuit.cur_valid.expect(cur_valid)
            tester.step(2)
            cur_valid += 1 if valid_clocks[j] else 0
    compile_and_run(tester)

def test_nested_counters_TSeq_3_1_TSeq_2_2_SSeq_3_reset_valid():
    no = 3
    io = 1
    ni = 2
    ii = 2
    nii = 3
    t = ST_TSeq(no, io, ST_TSeq(ni, ii, ST_SSeq(nii, ST_Int())))
    testcircuit = DefineNestedCounters(t, has_cur_valid=True, has_reset=True)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    num_clocks = (no+io) * (ni+ii)
    valid_clocks = [(o < no) and (i < ni) for o in range(no+io) for i in range(ni+ii)]

    tester.circuit.RESET = 0
    tester.eval()
    cur_clk = 0
    for k in range(2):
        cur_valid = 0
        for j in range((no+io) * (ni+ii)):
            tester.print("clk: {} \n".format(cur_clk))
            cur_clk += 1
            tester.print("valid: %d\n", tester.circuit.valid)
            tester.print("last: %d\n", tester.circuit.last)
            tester.circuit.valid.expect(valid_clocks[j])
            tester.circuit.last.expect(j == num_clocks - 1)
            if valid_clocks[j]:
                tester.circuit.cur_valid.expect(cur_valid)
            tester.step(2)
            cur_valid += 1 if valid_clocks[j] else 0
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
