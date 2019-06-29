from aetherling.modules.rshift import RShiftParallel, DefineRShiftSequential
from magma import *
from magma.clock import *
from magma.bitutils import *
from magma.simulator.coreir_simulator import CoreIRSimulator
from magma.scope import Scope
import fault

def test_rshift_parallel():
    width = 5
    num_in = 4
    test_vals = [2,5,3,8]
    shift_amount = 2
    in_type = Array[num_in, Array[width, In(BitIn)]]
    scope = Scope()
    args = ['I', in_type, 'O', Out(in_type)] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test', *args)

    rshift = RShiftParallel(num_in, shift_amount, in_type.T)
    wire(rshift.I, testcircuit.I)
    wire(testcircuit.O, rshift.O)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    for i, val in enumerate(test_vals):
        sim.set_value(testcircuit.I[i], int2seq(val, width), scope)
    sim.evaluate()
    for i, val in enumerate(test_vals[shift_amount:]):
        assert seq2int(sim.get_value(testcircuit.O[i + shift_amount])) == test_vals[i]

def fault_test_rshift_parallel():
    width = 5
    num_in = 4
    test_vals = [2,5,3,8]
    shift_amount = 2
    in_type = Array[num_in, Array[width, In(BitIn)]]
    args = ['I', in_type, 'O', Out(in_type)] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test', *args)

    rshift = RShiftParallel(num_in, shift_amount, in_type.T)
    wire(rshift.I, testcircuit.I)
    wire(testcircuit.O, rshift.O)

    EndCircuit()

    magma.compile("vBuild/" + testcircuit.name, testcircuit, output="coreir-verilog",
                  passes=["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst", "flattentypes", "flatten", "verifyconnectivity --noclkrst", "deletedeadinstances"],
                  namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    for i, val in enumerate(test_vals):
        tester.circuit.I[i] = val
    tester.eval()
    for i, val in enumerate(test_vals[shift_amount:]):
        tester.circuit.O[i + shift_amount].expect(test_vals[i])
    tester.compile_and_run(target="verilator", skip_compile=True, directory="vBuild/")

def test_rshift_parallel_alternate_rv():
    width = 5
    num_in = 4
    test_vals = [2,5,3,8]
    shift_amount = 2
    num_clocks = 6
    scope = Scope()
    in_type = Array[num_in, Array[width, In(BitIn)]]
    args = ['I', in_type, 'O', Out(in_type), 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test', *args)

    rshift = RShiftParallel(num_in, shift_amount, in_type.T, True)
    wire(rshift.I, testcircuit.I)
    wire(testcircuit.O, rshift.O)
    wire(testcircuit.ready_up, rshift.ready_up)
    wire(testcircuit.valid_up, rshift.valid_up)
    wire(testcircuit.ready_down, rshift.ready_down)
    wire(testcircuit.valid_down, rshift.valid_down)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    sim.evaluate()
    for clk in range(num_clocks):
        for i, val in enumerate(test_vals):
            sim.set_value(testcircuit.I[i], int2seq(val + clk * 3, width), scope)
        sim.set_value(testcircuit.valid_up, clk % 3 == 0, scope)
        sim.set_value(testcircuit.ready_down, clk % 2 == 0, scope)
        sim.evaluate()
        for i, val in enumerate(test_vals[shift_amount:]):
            assert seq2int(sim.get_value(testcircuit.O[i + shift_amount])) == test_vals[i] + clk * 3
        assert sim.get_value(testcircuit.valid_down, scope) == (clk % 3 == 0)
        assert sim.get_value(testcircuit.ready_up, scope) == (clk % 2 == 0)
        sim.advance_cycle()
        sim.evaluate()


def test_rshift_sequential_rv_always_true_no_ce():
    width = 5
    test_vals = [2,5,3,8]
    shift_amount = 1
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

