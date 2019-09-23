from aetherling.space_time import *
from magma import *
from magma.clock import *
from magma.bitutils import *
from magma.simulator.coreir_simulator import CoreIRSimulator
from magma.scope import Scope
import fault
from aetherling.helpers.fault_helpers import compile_and_run

def test_rshift_parallel():
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

def test_fault_rshift_parallel():
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

def test_rshift_sequential_v_always_true_no_ce():
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

def test_rshift_sequential_invalid_v_delayed_true_no_ce():
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

"""
def test_rshift_sequential_rv_and_ce_flicker():
    width = 5
    test_vals = [2,5,3,8]
    shift_amount = 1
    ready_down_period = 3
    valid_up_period = 4
    ce_period = 2
    # 12 is lcm of all periods
    lcm_periods = 12
    # rshift only executes when upstream valid, downstream valid, and ce high
    # that occurs only on lcm of their periods
    num_clocks_per_iteration = len(test_vals) * lcm_periods
    num_iterations = 2
    scope = Scope()

    testcircuit = DefineRShiftSequential(len(test_vals), 1, shift_amount, Array[width, Bit], has_ce=True)
    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)


    for i in range(num_iterations):
        for clk in range(num_clocks_per_iteration):
            sim.set_value(testcircuit.valid_up, clk % valid_up_period == 0, scope)
            sim.set_value(testcircuit.ready_down, clk % ready_down_period == 0, scope)
            sim.set_value(testcircuit.CE, clk % ce_period == 0, scope)
            if clk % valid_up_period == 0:
                sim.set_value(testcircuit.I, int2seq(test_vals[clk // lcm_periods], width), scope)
            # ensure that can change input on not valid_up clock and not affect anything
            else:
                sim.set_value(testcircuit.I, int2seq(0, width), scope)
            sim.evaluate()
            # check ready
            assert sim.get_value(testcircuit.ready_up, scope) == (clk % ready_down_period == 0 and
                                                                  clk % ce_period == 0)
            # check valid
            assert sim.get_value(testcircuit.valid_down, scope) == (clk % valid_up_period == 0 and
                                                                    clk % ce_period == 0)
            # check that output right
            if clk > 0 and clk % ce_period == 0 and clk % ready_down_period == 0 and \
                clk % valid_up_period == 0:
                assert seq2int(sim.get_value(testcircuit.O, scope)) == test_vals[(clk // lcm_periods) - 1]
            sim.advance_cycle()
            sim.evaluate()

def test_rshift_sequential_multi_clock_elements_rv_always_true_no_ce():
    width = 5
    test_vals = [2,5,3,8]
    shift_amount = 1
    time_per_element = 2
    num_clocks_per_iteration = len(test_vals)
    num_iterations = 2
    scope = Scope()

    testcircuit = DefineRShiftSequential(len(test_vals) // time_per_element, time_per_element, shift_amount, Array[width, Bit])
    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)


    sim.set_value(testcircuit.valid_up, True, scope)
    sim.set_value(testcircuit.ready_down, True, scope)
    for i in range(num_iterations):
        for clk in range(num_clocks_per_iteration):
            sim.set_value(testcircuit.I, int2seq(test_vals[clk] + i, width), scope)
            sim.evaluate()
            if clk >= shift_amount * time_per_element:
                assert seq2int(sim.get_value(testcircuit.O, scope)) == test_vals[clk - shift_amount * time_per_element] + i
            assert sim.get_value(testcircuit.ready_up, scope) == True
            assert sim.get_value(testcircuit.valid_down, scope) == True
            sim.advance_cycle()
            sim.evaluate()

def test_rshift_sequential_shift_great_than_1_rv_always_true_no_ce():
    width = 5
    test_vals = [2,5,3,8,1,9]
    shift_amount = 2
    num_clocks_per_iteration = len(test_vals)
    num_iterations = 2
    scope = Scope()

    testcircuit = DefineRShiftSequential(len(test_vals), 1, shift_amount, Array[width, Bit])
    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)


    sim.set_value(testcircuit.valid_up, True, scope)
    sim.set_value(testcircuit.ready_down, True, scope)
    for i in range(num_iterations):
        for clk in range(num_clocks_per_iteration):
            sim.set_value(testcircuit.I, int2seq(test_vals[clk] + i, width), scope)
            sim.evaluate()
            if clk >= shift_amount:
                assert seq2int(sim.get_value(testcircuit.O, scope)) == test_vals[clk - shift_amount] + i
            assert sim.get_value(testcircuit.ready_up, scope) == True
            assert sim.get_value(testcircuit.valid_down, scope) == True
            sim.advance_cycle()
            sim.evaluate()

def test_rshift_sequential_shift_great_than_1_multi_clock_elements_rv_always_true_no_ce():
    width = 5
    test_vals = [2,5,3,8,1,9]
    time_per_element = 2
    shift_amount = 2
    num_clocks_per_iteration = len(test_vals)
    num_iterations = 2
    scope = Scope()

    testcircuit = DefineRShiftSequential(len(test_vals) // time_per_element, time_per_element, shift_amount, Array[width, Bit])
    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)


    sim.set_value(testcircuit.valid_up, True, scope)
    sim.set_value(testcircuit.ready_down, True, scope)
    for i in range(num_iterations):
        for clk in range(num_clocks_per_iteration):
            sim.set_value(testcircuit.I, int2seq(test_vals[clk] + i, width), scope)
            sim.evaluate()
            if clk >= shift_amount * time_per_element:
                assert seq2int(sim.get_value(testcircuit.O, scope)) == test_vals[clk - shift_amount * time_per_element] + i
            assert sim.get_value(testcircuit.ready_up, scope) == True
            assert sim.get_value(testcircuit.valid_down, scope) == True
            sim.advance_cycle()
            sim.evaluate()

def test_rshift_sequential_shift_great_than_1_multi_clock_elements_rv_and_ce_flicker():
    width = 5
    test_vals = [2,5,3,8,1,9]
    time_per_element = 2
    shift_amount = 2
    ready_down_period = 3
    valid_up_period = 4
    ce_period = 2
    # 12 is lcm of all periods
    lcm_periods = 12
    # rshift only executes when upstream valid, downstream valid, and ce high
    # that occurs only on lcm of their periods
    num_clocks_per_iteration = len(test_vals) * lcm_periods
    num_iterations = 2
    scope = Scope()

    testcircuit = DefineRShiftSequential(len(test_vals) // time_per_element, time_per_element, shift_amount, Array[width, Bit], True)
    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)


    sim.set_value(testcircuit.valid_up, True, scope)
    sim.set_value(testcircuit.ready_down, True, scope)
    for i in range(num_iterations):
        for clk in range(num_clocks_per_iteration):
            sim.set_value(testcircuit.valid_up, clk % valid_up_period == 0, scope)
            sim.set_value(testcircuit.ready_down, clk % ready_down_period == 0, scope)
            sim.set_value(testcircuit.CE, clk % ce_period == 0, scope)
            if clk % valid_up_period == 0:
                sim.set_value(testcircuit.I, int2seq(test_vals[clk // lcm_periods] + i, width), scope)
            # ensure that can change input on not valid_up clock and not affect anything
            else:
                sim.set_value(testcircuit.I, int2seq(0, width), scope)
            sim.evaluate()
            # check ready
            assert sim.get_value(testcircuit.ready_up, scope) == (clk % ready_down_period == 0 and
                                                                  clk % ce_period == 0)
            # check valid
            assert sim.get_value(testcircuit.valid_down, scope) == (clk % valid_up_period == 0 and
                                                                    clk % ce_period == 0)
            # check that output right
            if clk >= shift_amount * time_per_element * lcm_periods and clk % ce_period == 0 and clk % ready_down_period == 0 and \
                    clk % valid_up_period == 0:
                assert seq2int(sim.get_value(testcircuit.O, scope)) == test_vals[(clk // lcm_periods) - shift_amount * time_per_element] + i
            sim.advance_cycle()
            sim.evaluate()
"""
