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
from aetherling.helpers.fault_helpers import print_start_clock, print_end_clock, print_nd_int_array_port, print_nd_bit_array_port

int_width = 8
# this computes the convlution that is computed at different throughputs by the
# differently scheduled Aetherling pipelines
num_rows = 256
num_cols = 256
image_matrix = [[row * num_rows + col for col in range(num_cols)] for row in range(num_rows)]
stencil = [[1,2],[3,4]]
# since this is a 2x2 stencil, the right most column and bottom most row are invalid data to be ignored
# these are only emitted so that the rate doesn't have any ugly constants
valid_in_rows = [x for x in range(0,num_rows - 1,2)]
valid_in_cols = [x for x in range(0,num_cols - 1,2)]
valid_after_first_conv_rows = [x for x in range(0,num_rows // 2 - 1,2)]
valid_after_first_conv_cols = [x for x in range(0,num_cols // 2 - 1,2)]
valid_after_second_conv_rows = [x for x in range(0,num_rows // 4 - 1,2)]
valid_after_second_conv_cols = [x for x in range(0,num_cols // 4 - 1,2)]

def do_convolution_at_point(row, col, input_matrix):
    return ((stencil[0][0] * input_matrix[row][col] + stencil[0][1] * input_matrix[row][col+1] +
    stencil[1][0] * input_matrix[row+1][col] + stencil[1][1] * input_matrix[row+1][col+1]) % 256 // 4)

firstResults = [[do_convolution_at_point(row,col, image_matrix) for col in valid_in_cols] for row in valid_in_rows]
secondResults = [[do_convolution_at_point(row,col, firstResults) for col in valid_after_first_conv_cols] for row in valid_after_first_conv_rows]
thirdResults = [[do_convolution_at_point(row,col, secondResults) for col in valid_after_second_conv_cols] for row in valid_after_second_conv_rows]
flattenedThirdResults = [element for sublist in thirdResults for element in sublist]

valid_out_rows = [x for x in range(len(thirdResults))]
valid_out_cols = [x for x in range(len(thirdResults[0]))]

num_valid_out_rows = len(valid_out_rows)
num_valid_out_cols = len(valid_out_rows)


def test_downsample_256x256_to_32x32_1px_in_per_clk():
    from .downsample_256x256_to_32x32_1px_in_per_clk import c, downsample_256x256_to_32x32_1px_in_per_clk as testcircuit

    magma.compile("vBuild/" + testcircuit.name, testcircuit, output="verilog",
                  passes=["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst", "flattentypes", "flatten", "verifyconnectivity --noclkrst", "deletedeadinstances"],
                  namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"], context = c)

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.poke(testcircuit.valid_data_in, 1)
    tester.poke(testcircuit.ready_data_out, 1)
    tester.poke(testcircuit.CE, 1)

    # these check the outputs, as there is a delay between feeding inputs in and getting the results back
    for row in range(num_rows+20):
        for col in range(0,num_cols):
            # a necessary adjustment as running tests for multiple clocks after inputting full image
            # to get rest of the outputs
            if row < num_rows:
                tester.poke(testcircuit.I0, image_matrix[row][col])
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
        filtered_results = [x['O0'] for x in results if x['valid'] == 1]
        if len(filtered_results) >= len(flattenedThirdResults):
            assert filtered_results[0:len(flattenedThirdResults)] == [element for sublist in thirdResults for element in sublist]
        else:
            assert filtered_results == [element for sublist in thirdResults for element in sublist]
