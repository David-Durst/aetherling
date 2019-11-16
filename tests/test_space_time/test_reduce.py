from aetherling.space_time import *
from magma import *
from magma.clock import *
from magma.bitutils import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run, wrap_module_with_top

def test_reduce_s_basic():
    num_in = 4
    num_iterations = 2
    test_vals = [2,4,6,8,1,3,5,9]
    outputs = [20,18]
    red = DefineReduce_S(num_in, DefineAdd_Atom(), has_valid=True)

    tester = fault.Tester(red, red.CLK)

    tester.circuit.valid_up = 1
    for i in range(num_iterations):
        tester.print("clk: {}\n".format(i))
        for j in range(num_in):
            tester.circuit.I[j] = test_vals[i*num_in + j]
        tester.step(2)
        tester.circuit.valid_down.expect(1)
        tester.circuit.O[0].expect(outputs[i])
    compile_and_run(tester)

def test_reduce_s_map_1_s():
    num_in = 4
    num_iterations = 2
    test_vals = [2,4,6,8,1,3,5,9]
    outputs = [20,18]
    red = wrap_module_with_top(DefineReduce_S(num_in, DefineMap_S(1, DefineAdd_Atom(), has_valid=False), has_valid=True))

    tester = fault.Tester(red, red.CLK)

    tester.circuit.valid_up = 1
    for i in range(num_iterations):
        tester.print("clk: {}\n".format(i))
        for j in range(num_in):
            tester.circuit.I[j][0] = test_vals[i*num_in + j]
        tester.step(2)
        tester.circuit.valid_down.expect(1)
        tester.circuit.O[0][0].expect(outputs[i])
    compile_and_run(tester)

def test_reduce_s_map_1_t_map_1_s():
    num_in = 4
    num_iterations = 2
    test_vals = [2,4,6,8,1,3,5,9]
    outputs = [20,18]
    red = wrap_module_with_top(DefineReduce_S(num_in, DefineMap_T(1, 1, DefineMap_S(1, DefineAdd_Atom(), has_valid=False)), has_valid=True))

    tester = fault.Tester(red, red.CLK)

    tester.circuit.valid_up = 1
    for i in range(num_iterations):
        tester.print("clk: {}\n".format(i))
        for j in range(num_in):
            tester.circuit.I[j][0] = test_vals[i*num_in + j]
        tester.eval()
        tester.step(2)
        tester.circuit.valid_down.expect(1)
        tester.circuit.O[0][0].expect(outputs[i])
        tester.step(2)
    compile_and_run(tester)

def test_reduce_s_map_1_s_map_1_s():
    num_in = 4
    num_iterations = 2
    test_vals = [2,4,6,8,1,3,5,9]
    outputs = [20,18]
    red = wrap_module_with_top(DefineReduce_S(num_in, DefineMap_S(1, DefineMap_S(1, DefineAdd_Atom(), has_valid=False), has_valid=False), has_valid=True))

    tester = fault.Tester(red, red.CLK)

    tester.circuit.valid_up = 1
    for i in range(num_iterations):
        tester.print("clk: {}\n".format(i))
        for j in range(num_in):
            tester.circuit.I[j][0][0] = test_vals[i*num_in + j]
        tester.step(2)
        tester.circuit.valid_down.expect(1)
        tester.circuit.O[0][0][0].expect(outputs[i])
        tester.step(2)
    compile_and_run(tester)

def test_reduce_t_basic():
    num_in = 4
    num_iterations = 2
    test_vals = [2,4,6,8,1,3,5,9]
    outputs = [20,18]
    red = DefineReduce_T(num_in, 0, DefineAdd_Atom())

    tester = fault.Tester(red, red.CLK)

    tester.circuit.valid_up = 1
    for i in range(num_iterations*num_in):
        tester.print("clk: {}\n".format(i))
        tester.circuit.I = test_vals[i]
        tester.eval()
        if i >= num_in:
            tester.circuit.valid_down.expect(1)
        if i % num_in == num_in:
            tester.circuit.O.expect(outputs[i // num_in])
        tester.step(2)
    compile_and_run(tester)

def test_reduce_t_map_1_s():
    num_in = 4
    num_iterations = 2
    test_vals = [2,4,6,8,1,3,5,9]
    outputs = [20,18]
    red = wrap_module_with_top(DefineReduce_T(num_in, 0, DefineMap_S(1, DefineAdd_Atom(), has_valid=False)))

    tester = fault.Tester(red, red.CLK)

    tester.circuit.valid_up = 1
    for i in range(num_iterations*num_in):
        tester.print("clk: {}\n".format(i))
        tester.circuit.I[0] = test_vals[i]
        tester.eval()
        if i >= num_in:
            tester.circuit.valid_down.expect(1)
        if i % num_in == num_in:
            tester.circuit.O[0].expect(outputs[i // num_in])
        tester.step(2)
    compile_and_run(tester)

def test_reduce_t_map_1_t_map_1_s():
    num_in = 4
    num_iterations = 2
    test_vals = [2,4,6,8,1,3,5,9]
    outputs = [20,18]
    red = wrap_module_with_top(DefineReduce_T(num_in, 0, DefineMap_T(1, 1, DefineMap_S(1, DefineAdd_Atom(), has_valid=False))))

    tester = fault.Tester(red, red.CLK)

    tester.circuit.valid_up = 1
    for i in range(num_iterations*num_in):
        tester.print("clk: {}\n".format(i))
        tester.circuit.I[0] = test_vals[i]
        tester.eval()
        if i >= num_in:
            tester.circuit.valid_down.expect(1)
        if i % num_in == num_in:
            tester.circuit.O[0].expect(outputs[i // num_in])
        tester.step(4)
    compile_and_run(tester)

def test_reduce_t_map_1_2_t_map_1_s():
    num_in = 4
    num_iterations = 2
    test_vals = [2,4,6,8,1,3,5,9]
    outputs = [20,18]
    red = wrap_module_with_top(DefineReduce_T(num_in, 0, DefineMap_T(1, 2, DefineMap_S(1, DefineAdd_Atom(), has_valid=False))))

    tester = fault.Tester(red, red.CLK)

    tester.circuit.valid_up = 1
    for i in range(num_iterations*num_in):
        tester.print("clk: {}\n".format(i))
        tester.circuit.I[0] = test_vals[i]
        tester.eval()
        if i >= num_in:
            tester.circuit.valid_down.expect(1)
        if i % num_in == num_in:
            tester.circuit.O[0].expect(outputs[i // num_in])
        tester.step(2)
        tester.circuit.valid_down.expect(i >= num_in-1)
        tester.step(2)
        tester.circuit.valid_down.expect(i >= num_in-1)
        tester.step(2)
    compile_and_run(tester)

def test_reduce_t_map_1_s_map_1_s():
    num_in = 4
    num_iterations = 2
    test_vals = [2,4,6,8,1,3,5,9]
    outputs = [20,18]
    red = wrap_module_with_top(DefineReduce_T(num_in, 0, DefineMap_S(1, DefineMap_S(1, DefineAdd_Atom(), has_valid=False), has_valid=False)))

    tester = fault.Tester(red, red.CLK)

    tester.circuit.valid_up = 1
    for i in range(num_iterations*num_in):
        tester.print("clk: {}\n".format(i))
        tester.circuit.I[0][0] = test_vals[i]
        tester.eval()
        if i >= num_in:
            tester.circuit.valid_down.expect(1)
        if i % num_in == num_in:
            tester.circuit.O[0][0].expect(outputs[i // num_in])
        tester.step(2)
    compile_and_run(tester)
