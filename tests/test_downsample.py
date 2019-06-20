from aetherling.modules.downsample import DownsampleSequential, DownsampleParallel
from magma.frontend.coreir_ import GetCoreIRModule
from magma import *
from magma.clock import *
from magma.backend.coreir_ import CoreIRBackend
from magma.bitutils import *
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope


def test_downsample_parallel():
    width = 5
    numIn = 2
    testVal = 3
    scope = Scope()
    outType = Array[width, Out(BitIn)]
    inType = Array[numIn, In(outType)]
    args = ['I', inType, 'O', outType] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test_Downsample_Parallel', *args)

    downsampleParallel = DownsampleParallel(numIn, 0, inType.T)
    wire(downsampleParallel.I, testcircuit.I)
    wire(testcircuit.O, downsampleParallel.O)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(numIn):
        sim.set_value(testcircuit.I[i], int2seq(testVal, width), scope)
    sim.evaluate()
    assert seq2int(sim.get_value(testcircuit.O, scope)) == testVal

def test_downsample_parallel_non_zero_idx():
    width = 5
    numIn = 4
    scope = Scope()
    outType = Array[width, Out(BitIn)]
    inType = Array[numIn, In(outType)]
    args = ['I', inType, 'O', outType] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test_Downsample_Parallel', *args)

    downsampleParallel = DownsampleParallel(numIn, 2, inType.T)
    wire(downsampleParallel.I, testcircuit.I)
    wire(testcircuit.O, downsampleParallel.O)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for i in range(numIn):
        sim.set_value(testcircuit.I[i], int2seq(i, width), scope)
    sim.evaluate()
    assert seq2int(sim.get_value(testcircuit.O, scope)) == 2

def test_downsample_parallel_rv():
    width = 5
    num_in_per_clk = 4
    num_clocks = 5
    scope = Scope()
    outType = Array[width, Out(BitIn)]
    inType = Array[num_in_per_clk, In(outType)]
    args = ['I', inType, 'O', outType, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(False, False)

    testcircuit = DefineCircuit('Test_Downsample_Parallel', *args)

    downsampleParallel = DownsampleParallel(num_in_per_clk, 2, inType.T, has_ready_valid=True)
    wire(downsampleParallel.I, testcircuit.I)
    wire(testcircuit.O, downsampleParallel.O)
    wire(testcircuit.ready_up, downsampleParallel.ready_up)
    wire(testcircuit.valid_up, downsampleParallel.valid_up)
    wire(testcircuit.ready_down, downsampleParallel.ready_down)
    wire(testcircuit.valid_down, downsampleParallel.valid_down)

    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for clk in range(num_clocks):
        sim.set_value(testcircuit.valid_up, clk % 4 == 0, scope)
        sim.set_value(testcircuit.ready_down, clk % 2 == 0, scope)
        for i in range(num_in_per_clk):
            sim.set_value(testcircuit.I[i], int2seq(i + clk * num_in_per_clk, width), scope)
        sim.evaluate()
        assert seq2int(sim.get_value(testcircuit.O, scope)) == 2 + clk * num_in_per_clk
        assert sim.get_value(testcircuit.valid_down, scope) == (clk % 4 == 0)
        assert sim.get_value(testcircuit.ready_up, scope) == (clk % 2 == 0)
        sim.advance_cycle()
        sim.evaluate()

def test_downsample_sequential_ce_and_rv_always_true():
    width = 5
    num_elements = 6
    active_idx = 3
    time_per_element = 1
    scope = Scope()
    out_type = Array[width, Out(BitIn)]
    in_type = In(out_type)
    args = ['I', in_type, 'O', out_type, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(has_ce=True)

    testcircuit = DefineCircuit('Test_Downsample_Sequential', *args)

    downsampleSequential = DownsampleSequential(num_elements, time_per_element, active_idx, in_type, has_ce=True)
    wire(downsampleSequential.I, testcircuit.I)
    wire(testcircuit.O, downsampleSequential.O)
    wire(testcircuit.ready_up, downsampleSequential.ready_up)
    wire(testcircuit.valid_up, downsampleSequential.valid_up)
    wire(testcircuit.ready_down, downsampleSequential.ready_down)
    wire(testcircuit.valid_down, downsampleSequential.valid_down)
    wire(testcircuit.CE, downsampleSequential.CE)
    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    for clk in range(num_elements):
        sim.set_value(testcircuit.valid_up, True, scope)
        sim.set_value(testcircuit.ready_down, True, scope)
        sim.set_value(testcircuit.CE, True, scope)
        sim.set_value(testcircuit.I, int2seq(clk, width), scope)
        sim.evaluate()
        if clk == active_idx:
            assert seq2int(sim.get_value(testcircuit.O, scope)) == clk
        assert sim.get_value(testcircuit.valid_down, scope) == (active_idx == clk // time_per_element)
        assert sim.get_value(testcircuit.ready_up, scope) == True
        sim.advance_cycle()
        sim.evaluate()

def test_downsample_sequential_ce_and_rv_flicker():
    width = 5
    num_elements = 6
    active_idx = 3
    time_per_element = 1
    scope = Scope()
    out_type = Array[width, Out(BitIn)]
    in_type = In(out_type)
    args = ['I', in_type, 'O', out_type, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(has_ce=True)

    testcircuit = DefineCircuit('Test_Downsample_Sequential', *args)

    downsampleSequential = DownsampleSequential(num_elements, time_per_element, active_idx, in_type, has_ce=True)
    wire(downsampleSequential.I, testcircuit.I)
    wire(testcircuit.O, downsampleSequential.O)
    wire(testcircuit.ready_up, downsampleSequential.ready_up)
    wire(testcircuit.valid_up, downsampleSequential.valid_up)
    wire(testcircuit.ready_down, downsampleSequential.ready_down)
    wire(testcircuit.valid_down, downsampleSequential.valid_down)
    wire(testcircuit.CE, downsampleSequential.CE)
    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    # * 12 as only valid every 12th clock, *2 as iterating over sequence twice
    for clk in range(num_elements * 12 * 2):
        sim.set_value(testcircuit.valid_up, clk % 2 == 0, scope)
        sim.set_value(testcircuit.ready_down, clk % 3 == 0, scope)
        sim.set_value(testcircuit.CE, clk % 4 == 0, scope)
        sim.set_value(testcircuit.I, int2seq(clk // 12, width), scope)
        sim.evaluate()
        if clk // 12 == active_idx and clk % 12 == 0:
            assert seq2int(sim.get_value(testcircuit.O, scope)) == clk // 12
        # 2 possible active_idx as doing downsample over two input sequences
        assert sim.get_value(testcircuit.valid_down, scope) == ((clk // 12 in [active_idx, num_elements + active_idx]) & (clk % 12 == 0))
        assert sim.get_value(testcircuit.ready_up, scope) == (clk % 3 == 0)
        sim.advance_cycle()
        sim.evaluate()

def test_downsample_sequential_multi_clock_elements():
    width = 5
    num_elements = 6
    active_idx = 3
    time_per_element = 3
    scope = Scope()
    out_type = Array[width, Out(BitIn)]
    in_type = In(out_type)
    args = ['I', in_type, 'O', out_type, 'ready_up', Out(Bit), 'valid_up', In(Bit), 'ready_down', In(Bit),
            'valid_down', Out(Bit)] + ClockInterface(has_ce=True)

    testcircuit = DefineCircuit('Test_Downsample_Sequential', *args)

    downsampleSequential = DownsampleSequential(num_elements, time_per_element, active_idx, in_type, has_ce=True)
    wire(downsampleSequential.I, testcircuit.I)
    wire(testcircuit.O, downsampleSequential.O)
    wire(testcircuit.ready_up, downsampleSequential.ready_up)
    wire(testcircuit.valid_up, downsampleSequential.valid_up)
    wire(testcircuit.ready_down, downsampleSequential.ready_down)
    wire(testcircuit.valid_down, downsampleSequential.valid_down)
    wire(testcircuit.CE, downsampleSequential.CE)
    EndCircuit()

    sim = CoreIRSimulator(testcircuit, testcircuit.CLK,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    # * 12 as only valid every 12th clock
    valid_clks = [i for i in range(num_elements * 12 * time_per_element) \
                  if i // 12 >= active_idx * time_per_element and \
                  i // 12 < (active_idx + 1) * time_per_element and \
                  i % 12 == 0]

    # duplicating to repeat the input sequence
    valid_clks += [num_elements * 12 * time_per_element + i for i in valid_clks]

    for clk in range(num_elements * 12 * time_per_element * 2):
        sim.set_value(testcircuit.valid_up, clk % 2 == 0, scope)
        sim.set_value(testcircuit.ready_down, clk % 3 == 0, scope)
        sim.set_value(testcircuit.CE, clk % 4 == 0, scope)
        sim.set_value(testcircuit.I, int2seq(clk // (12 * time_per_element), width), scope)
        sim.evaluate()
        if clk in valid_clks:
            assert seq2int(sim.get_value(testcircuit.O, scope)) == clk // (12 * time_per_element)
        assert sim.get_value(testcircuit.valid_down, scope) == (clk in valid_clks)
        assert sim.get_value(testcircuit.ready_up, scope) == (clk % 3 == 0)
        sim.advance_cycle()
        sim.evaluate()

