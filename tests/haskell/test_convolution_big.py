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

def run_conv_test(testcircuit, c, parallelism):
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
            for i in range(parallelism):
                print_nd_int_array_port(tester, getattr(testcircuit, "O" + str(i)), testcircuit.O0, "O" + str(i))
            print_end_clock(tester, testcircuit.O0)
    tester.compile_and_run(target="verilator", skip_compile=True, directory="vBuild/")
    with open(f"vBuild/obj_dir/{testcircuit.name}.log") as file:
        results = eval("[" + file.read() + "]")
        filtered_results = []
        current_output = 0
        for clk_result in results:
            if clk_result['valid'] == 1:
                current_output_row = current_output // num_rows
                current_output_column = current_output % num_rows
                for i in range(parallelism):
                    if current_output_row in valid_rows and current_output_column + i in valid_cols:
                        filtered_results.append(clk_result["O" + str(i)])
                current_output += parallelism
        if len(filtered_results) >= len(flattenedResults):
            assert filtered_results[0:len(flattenedResults)] == flattenedResults
        else:
            assert filtered_results == flattenedResults

def test_convolution_32x32Im_2x2Win_1px_in_per_clk():
    from .convolution_32x32Im_2x2Win_1px_in_per_clk import c, convolution_32x32Im_2x2Win_1px_in_per_clk as testcircuit
    run_conv_test(testcircuit, c, 1)

def test_convolution_32x32Im_2x2Win_2px_in_per_clk():
    from .convolution_32x32Im_2x2Win_2px_in_per_clk import c, convolution_32x32Im_2x2Win_2px_in_per_clk as testcircuit
    run_conv_test(testcircuit, c, 2)

def test_convolution_32x32Im_2x2Win_4px_in_per_clk():
    from .convolution_32x32Im_2x2Win_4px_in_per_clk import c, convolution_32x32Im_2x2Win_4px_in_per_clk as testcircuit
    run_conv_test(testcircuit, c, 4)

def test_convolution_32x32Im_2x2Win_8px_in_per_clk():
    from .convolution_32x32Im_2x2Win_8px_in_per_clk import c, convolution_32x32Im_2x2Win_8px_in_per_clk as testcircuit
    run_conv_test(testcircuit, c, 8)

