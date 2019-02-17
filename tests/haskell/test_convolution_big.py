from aetherling.modules.reduce import ReduceSequential, ReduceParallel, ReducePartiallyParallel, renameCircuitForReduce
from magma.backend.coreir_ import CoreIRBackend
from magma.bitutils import *
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from mantle.coreir.arith import *
from mantle.coreir import DefineCoreirConst
import fault
from aetherling.helpers.fault_helpers import *

int_width = 8
# this computes the convlution that is computed at different throughputs by the
# differently scheduled Aetherling pipelines
num_rows = 32
num_cols = 32
image_matrix = [[row * num_rows + col for col in range(num_cols)] for row in range(num_rows)]
stencil = [[1,2],[2,1]]
# since this is a 2x2 stencil, the right most column and bottom most row are invalid data to be ignored
# these are only emitted so that the rate doesn't have any ugly constants
valid_rows = [x for x in range(num_rows - 1)]
valid_cols = [x for x in range(num_cols - 1)]

def do_convolution_at_point(row, col):
    return ((stencil[0][0] * image_matrix[row][col] + stencil[0][1] * image_matrix[row][col+1] +
    stencil[1][0] * image_matrix[row+1][col] + stencil[1][1] * image_matrix[row+1][col+1]) % 256)

results = [[do_convolution_at_point(row,col) for col in valid_cols] for row in valid_rows]
flattenedResults = [element for sublist in results for element in sublist]

def test_sequential_convolution():
    from .sequentialConvolution import cirb as sequentialConvolutionCirb, sequentialConvolution
    scope = Scope()
    sim = CoreIRSimulator(sequentialConvolution, sequentialConvolution.CLK, context=sequentialConvolutionCirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.set_value(sequentialConvolution.valid_data_in, 1, scope)
    sim.set_value(sequentialConvolution.ready_data_out, 1, scope)
    sim.set_value(sequentialConvolution.CE, 1, scope)

    # these check the outputs, as there is a delay between feeding inputs in and getting the results back
    cur_row_to_check = 0
    cur_col_to_check = 0
    successfully_checked_all_valid_outputs = False
    for row in range(num_rows+3):
        for col in range(num_cols):
            # a necessary adjustment as running tests for multiple clocks after inputting full image
            # to get rest of the outputs
            if row < num_rows:
                sim.set_value(sequentialConvolution.I0, int2seq(image_matrix[row][col], int_width))
            sim.evaluate()
            assert sim.get_value(sequentialConvolution.ready_data_in, scope) == 1
            if sim.get_value(sequentialConvolution.valid_data_out, scope) == 1:
                if cur_row_to_check in valid_rows and cur_col_to_check in valid_cols:
                    if seq2int(sim.get_value(sequentialConvolution.O0, scope)) != results[cur_row_to_check][cur_col_to_check]:
                        print(cur_col_to_check)
                    assert seq2int(sim.get_value(sequentialConvolution.O0, scope)) == results[cur_row_to_check][cur_col_to_check]
                if not cur_row_to_check in valid_rows and not cur_col_to_check in valid_cols:
                    successfully_checked_all_valid_outputs = True
                    break
                cur_col_to_check += 1
                cur_col_to_check = cur_col_to_check % num_cols
                if cur_col_to_check == 0:
                    cur_row_to_check += 1
                    cur_row_to_check = cur_row_to_check % num_rows
            sim.advance_cycle()
        if successfully_checked_all_valid_outputs:
            break
    assert successfully_checked_all_valid_outputs

def test_convolution_32x32Im_2x2Win_1px_in_per_clk():
    from .convolution_32x32Im_2x2Win_1px_in_per_clk import c, convolution_32x32Im_2x2Win_1px_in_per_clk as testcircuit
    parallelism = 1

    magma.compile("vBuild/" + testcircuit.name, testcircuit, output="verilog",
                  passes=["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst", "flattentypes", "flatten", "verifyconnectivity --noclkrst", "deletedeadinstances"],
                  namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"], context = c)

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.poke(testcircuit.valid_data_in, 1)
    tester.poke(testcircuit.ready_data_out, 1)
    tester.poke(testcircuit.CE, 1)

    # these check the outputs, as there is a delay between feeding inputs in and getting the results back
    for row in range(num_rows+20):
        for col in range(0,num_cols, parallelism):
            # a necessary adjustment as running tests for multiple clocks after inputting full image
            # to get rest of the outputs
            if row < num_rows:
                for i in range(parallelism):
                    tester.poke(getattr(testcircuit, "I" + str(i)), image_matrix[row][col + i])
            tester.eval()
            tester.step(2)
            tester.eval()
            print_start_clock(tester, testcircuit.O0)
            print_nd_bit_array_port(tester, testcircuit.valid_data_out, testcircuit.O0, "valid")
            print_nd_int_array_port(tester, testcircuit.O0, testcircuit.O0, "O0")
            print_end_clock(tester, testcircuit.O0)
    tester.compile_and_run(target="verilator", skip_compile=True, directory="vBuild/")
    with open(f"vBuild/obj_dir/{testcircuit.name}.log") as file:
        results = eval("[" + file.read() + "]")
        filtered_results = []
        current_output = 0
        for clk_result in results:
            if clk_result['valid'] == 1:
                current_output_row = current_output % num_rows
                current_output_column = current_output // num_rows
                current_output += 1
                if current_output_row in valid_rows and current_output_column in valid_cols:
                    for i in range(parallelism):
                        filtered_results.append(clk_result["O" + str(i)])
        if len(filtered_results) >= len(flattenedResults):
            assert filtered_results[0:len(flattenedResults)] == flattenedResults
        else:
            assert filtered_results == flattenedResults
