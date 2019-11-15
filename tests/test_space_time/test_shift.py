from aetherling.space_time import *
from magma import *
from magma.clock import *
from magma.bitutils import *
from magma.simulator.coreir_simulator import CoreIRSimulator
from magma.scope import Scope
import fault
from aetherling.space_time.type_helpers import flatten
from aetherling.helpers.fault_helpers import compile_and_run, set_nested_port, \
    expect_nested_port, print_nested_port, int_to_ignore
import builtins

def test_shift_s():
    num_in = 4
    test_vals = [2,5,3,8]
    shift_amount = 2
    in_type = ST_SSeq(num_in, ST_Int())
    scope = Scope()
    args = ['I', In(in_type.magma_repr()), 'O', Out(in_type.magma_repr())] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test', *args)

    rshift = DefineShift_S(num_in, shift_amount, in_type.t)()
    wire(rshift.I, testcircuit.I)
    wire(testcircuit.O, rshift.O)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    for i, val in enumerate(test_vals):
        sim.set_value(testcircuit.I[i], int2seq(val, 8), scope)
    sim.evaluate()
    for i, val in enumerate(test_vals[shift_amount:]):
        assert seq2int(sim.get_value(testcircuit.O[i + shift_amount])) == test_vals[i]

def test_fault_shift_s():
    num_in = 4
    test_vals = [2,5,3,8]
    shift_amount = 2
    in_type = ST_SSeq(num_in, ST_Int())

    rshift = DefineShift_S(num_in, shift_amount, in_type.t, has_valid=True)

    tester = fault.Tester(rshift, rshift.CLK)

    tester.circuit.valid_up = 1
    for i, val in enumerate(test_vals):
        tester.circuit.I[i] = val
    tester.eval()
    for i, val in enumerate(test_vals[shift_amount:]):
        tester.circuit.O[i + shift_amount].expect(test_vals[i])
    tester.circuit.valid_down.expect(1)
    compile_and_run(tester)

def test_shift_t_v_always_true_no_ce():
    num_in = 4
    test_vals = [2,5,3,8]
    shift_amount = 1
    in_type = ST_TSeq(num_in, 0, ST_Int())
    num_clocks_per_iteration = len(test_vals)
    num_iterations = 2

    testcircuit = DefineShift_T(in_type.n, in_type.i, shift_amount, in_type.t, has_valid=True)

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.circuit.valid_up = 1
    for i in range(num_iterations):
        for clk in range(num_clocks_per_iteration):
            tester.circuit.I = test_vals[clk] + i
            tester.eval()
            if clk >= shift_amount:
                tester.circuit.O.expect(test_vals[clk - shift_amount] + i)
            tester.circuit.valid_down.expect(1)
            tester.step(2)
    compile_and_run(tester)

def test_shift_t_invalid_v_delayed_true_no_ce():
    delay = 3
    num_in = 5
    test_vals = [2,5,3,8,10]
    shift_amount = 1
    in_type = ST_TSeq(num_in, 1, ST_Int())
    num_clocks_per_iteration = num_in
    num_iterations = 2

    testcircuit = DefineShift_T(in_type.n, in_type.i, shift_amount, in_type.t, has_valid=True)

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.circuit.valid_up = 0
    for i in range(delay):
        tester.step(2)
        tester.circuit.valid_down.expect(0)
    tester.circuit.valid_up = 1
    for i in range(num_iterations):
        for clk in range(num_clocks_per_iteration):
            val_idx = min(clk, len(test_vals) - 1)
            tester.circuit.I = test_vals[val_idx] + i
            tester.eval()
            if clk >= shift_amount:
                tester.circuit.O.expect(test_vals[val_idx - shift_amount] + i)
            tester.circuit.valid_down.expect(1)
            tester.step(2)
    compile_and_run(tester)

def test_shift_t_invalid_v_delayed_true_ce():
    delay = 3
    num_in = 5
    test_vals = [2,5,3,8,10]
    shift_amount = 1
    in_type = ST_TSeq(num_in, 1, ST_Int())
    num_clocks_per_iteration = num_in + 1
    num_iterations = 2

    testcircuit = DefineShift_T(in_type.n, in_type.i, shift_amount, in_type.t, has_ce=True, has_valid=True)

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.circuit.CE = 1
    tester.circuit.valid_up = 0
    for i in range(delay):
        tester.step(2)
        tester.circuit.valid_down.expect(0)
    tester.circuit.valid_up = 1
    for i in range(num_iterations):
        for clk in range(num_clocks_per_iteration):
            tester.print("CLK: {}\n".format(clk))
            val_idx = min(clk, len(test_vals) - 1)
            tester.circuit.I = test_vals[val_idx] + i
            tester.eval()
            if clk >= shift_amount and clk < num_in:
                tester.circuit.O.expect(test_vals[val_idx - shift_amount] + i)
            tester.circuit.valid_down.expect(1)
            tester.step(2)
    compile_and_run(tester)

def test_shift_tt_v_always_true_no_ce():
    num_in = [3,2]
    test_vals = [2,5,7,3,8,9]
    shift_amount = 1
    in_type = ST_TSeq(num_in[0], 0, ST_TSeq(num_in[1], 0, ST_Int()))
    num_clocks_per_iteration = len(test_vals)
    num_iterations = 2

    testcircuit = DefineShift_TT(in_type.n, in_type.t.n, in_type.i, in_type.t.i, shift_amount, in_type.t.t, has_valid=True)

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.circuit.valid_up = 1
    for i in range(num_iterations):
        for clk in range(num_clocks_per_iteration):
            tester.print("CLK: {}\n".format(clk))
            tester.circuit.I = test_vals[clk] + i
            tester.eval()
            if clk >= shift_amount:
                tester.circuit.O.expect(test_vals[clk - shift_amount] + i)
            tester.circuit.valid_down.expect(1)
            tester.step(2)
    compile_and_run(tester)

def test_shift_tt_inner_invalid_v_always_true_no_ce():
    num_in = [3,2]
    test_vals = [2,5,7,3,8,9]
    shift_amount = 1
    ii = 2
    in_type = ST_TSeq(num_in[0], 0, ST_TSeq(num_in[1], ii, ST_Int()))
    num_iterations = 2

    testcircuit = DefineShift_TT(in_type.n, in_type.t.n, in_type.i, in_type.t.i, shift_amount, in_type.t.t, has_valid=True)

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.circuit.valid_up = 1
    for i in range(num_iterations):
        for clk_outer in range(num_in[0]):
            for clk_inner in range(num_in[1] + ii):
                tester.print("clk_outer: {}\n".format(clk_outer))
                tester.print("clk_inner: {}\n".format(clk_inner))
                val_idx = clk_outer * 2 + min(clk_inner, num_in[1] - 1)
                in_val = test_vals[val_idx] + i + (0 if clk_inner < 2 else 32)
                tester.circuit.I = in_val
                tester.print("I: {}\n".format(str(in_val)))
                tester.eval()
                tester.print("O: %d\n", tester.circuit.O)
                if val_idx >= shift_amount and clk_inner < 2:
                    tester.circuit.O.expect(test_vals[val_idx - shift_amount] + i)
                tester.circuit.valid_down.expect(1)
                tester.step(2)
    compile_and_run(tester)

def test_shift_ts():
    no = 2
    io = 0
    ni = 4
    test_vals = [[0,1,2,3], [4,5,6,7]]
    shifted_test_vals = [[int_to_ignore, int_to_ignore, 0, 1], [2, 3, 4, 5]]
    shift_amount = 2
    in_type = ST_TSeq(no, io, ST_SSeq(ni, ST_Int()))

    rshift = DefineShift_TS(no, io, ni, shift_amount, in_type.t.t, has_valid=True)

    tester = fault.Tester(rshift, rshift.CLK)

    tester.circuit.valid_up = 1
    for clk in range(len(test_vals)):
        tester.print("clk: {}".format(clk))
        set_nested_port(tester, tester.circuit.I, test_vals[clk], 1, 0)
        print_nested_port(tester, tester.circuit.I, 1)
        tester.print("\n")
        tester.eval()
        #for i, val in enumerate(test_vals[i*ni+shift_amount:(i+1)*ni+shift_amount]):
        print_nested_port(tester, tester.circuit.O, 1)
        tester.print("\n")
        expect_nested_port(tester, tester.circuit.O, shifted_test_vals[clk], 1, 0)
        tester.circuit.valid_down.expect(1)
        tester.step(2)
    compile_and_run(tester)
