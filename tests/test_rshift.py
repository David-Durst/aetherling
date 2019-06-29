from aetherling.modules.rshift import RShiftParallel, DefineRShiftSequential
from magma import *
from magma.clock import *
from magma.bitutils import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import fault

def test_rshift_parallel():
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

"""
def test_up_parallel_alternate_rv():
    width = 5
    numOut = 2
    testVal = 3
    num_clocks = 12
    scope = Scope()
    inType = Array[width, In(BitIn)]
    outType = Array[numOut, Out(inType)]
    args = ['I', inType, 'O', outType, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test', *args)

    upParallel = UpsampleParallel(numOut, inType, has_ready_valid=True)
    wire(upParallel.I, testcircuit.I)
    wire(testcircuit.O, upParallel.O)
    wire(testcircuit.ready_up, upParallel.ready_up)
    wire(testcircuit.valid_up, upParallel.valid_up)
    wire(testcircuit.ready_down, upParallel.ready_down)
    wire(testcircuit.valid_down, upParallel.valid_down)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK)

    sim.set_value(testcircuit.I, int2seq(testVal, width), scope)
    for clk in range(num_clocks):
        sim.set_value(testcircuit.valid_up, clk % 3 == 0, scope)
        sim.set_value(testcircuit.ready_down, clk % 2 == 0, scope)
        sim.evaluate()
        for i in range(numOut):
            assert seq2int(sim.get_value(testcircuit.O[i], scope)) == testVal
        assert sim.get_value(testcircuit.valid_down, scope) == (clk % 3 == 0)
        assert sim.get_value(testcircuit.ready_up, scope) == (clk % 2 == 0)
        sim.advance_cycle()
        sim.evaluate()
"""
