from aetherling.space_time import *
from magma import *
from magma.clock import *
from magma.bitutils import *
import fault
from aetherling.helpers.fault_helpers import compile_and_run

def test_fifo_basic():
    num_in = 4
    num_iterations = 2
    test_vals = [2,4,6,8,1,3,5,9]
    delay_amount = 2
    in_type = ST_TSeq(num_in, 0, ST_Int())

    rshift = DefineFIFO(in_type, delay_amount, has_valid=True)

    tester = fault.Tester(rshift, rshift.CLK)

    tester.circuit.valid_up = 1
    input_counter = 0
    output_counter = 0
    for i in range(num_iterations):
        for j in range(num_in):
            tester.print("clk: {}\n".format(i*num_iterations + j))
            tester.circuit.I = test_vals[input_counter]
            tester.eval()
            if i == 0 and j < delay_amount:
                tester.circuit.valid_down.expect(0)
            else:
                tester.circuit.valid_down.expect(1)
                tester.circuit.O.expect(test_vals[output_counter])
                output_counter = min(output_counter + 1, len(test_vals))
            input_counter = min(input_counter + 1, len(test_vals))
            tester.step(2)
    tester.circuit.valid_down.expect(1)
    compile_and_run(tester)

def test_fifo_invalids():
    num_in = 4
    num_iterations = 2
    test_vals = [2,4,6,8,0,1,3,5,9,0]
    delay_amount = 2
    in_type = ST_TSeq(num_in, 1, ST_Int())

    rshift = DefineFIFO(in_type, delay_amount, has_ce=True, has_valid=True)

    tester = fault.Tester(rshift, rshift.CLK)

    tester.circuit.valid_up = 1
    tester.circuit.CE = 1
    input_counter = 0
    output_counter = 0
    for i in range(num_iterations):
        for j in range(num_in):
            tester.print("clk: {}\n".format(i*num_iterations + j))
            tester.circuit.I = test_vals[input_counter]
            tester.eval()
            if i == 0 and j < delay_amount:
                tester.circuit.valid_down.expect(0)
            else:
                tester.circuit.valid_down.expect(1)
                if (output_counter % (num_in + 1) < num_in):
                    tester.circuit.O.expect(test_vals[output_counter])
                output_counter = min(output_counter + 1, len(test_vals))
            input_counter = min(input_counter + 1, len(test_vals))
            tester.step(2)
    tester.circuit.valid_down.expect(1)
    compile_and_run(tester)

def test_fifo_invalids_initial_delay():
    delay = 7
    num_in = 4
    num_iterations = 2
    test_vals = [2,4,6,8,0,0,1,3,5,9,0,0]
    delay_amount = 2
    invalids = 2
    in_type = ST_TSeq(num_in, invalids, ST_Int())

    rshift = DefineFIFO(in_type, delay_amount, has_ce=True, has_valid=True)

    tester = fault.Tester(rshift, rshift.CLK)

    tester.circuit.CE = 1
    tester.circuit.valid_up = 0
    for i in range(delay):
        tester.step(2)
        tester.circuit.valid_down.expect(0)
    tester.circuit.valid_up = 1
    input_counter = 0
    output_counter = 0
    for i in range(num_iterations):
        for j in range(num_in):
            tester.print("clk: {}\n".format(i*num_iterations + j))
            tester.circuit.I = test_vals[input_counter]
            tester.eval()
            if i == 0 and j < delay_amount:
                tester.circuit.valid_down.expect(0)
            else:
                tester.circuit.valid_down.expect(1)
                if (output_counter % (num_in + invalids) < num_in):
                    tester.circuit.O.expect(test_vals[output_counter])
                output_counter = min(output_counter + 1, len(test_vals))
            input_counter = min(input_counter + 1, len(test_vals))
            tester.step(2)
    tester.circuit.valid_down.expect(1)
    compile_and_run(tester)
