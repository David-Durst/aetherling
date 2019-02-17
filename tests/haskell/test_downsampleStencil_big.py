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

# note: the reason these tests don't have to worr about extra invalid values at end of output that are marked as valid
# is that, for a 2x2 window, only the last window is invalid. Since downsampleing by 2 in x dimension, that never
# gets emitted and only valid outputs are emitted.
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

def test_downsample_256x256_to_32x32_2px_in_per_clk():
    from .downsample_256x256_to_32x32_2px_in_per_clk import c, downsample_256x256_to_32x32_2px_in_per_clk as testcircuit

    magma.compile("vBuild/" + testcircuit.name, testcircuit, output="verilog",
                  passes=["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst", "flattentypes", "flatten", "verifyconnectivity --noclkrst", "deletedeadinstances"],
                  namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"], context = c)

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.poke(testcircuit.valid_data_in, 1)
    tester.poke(testcircuit.ready_data_out, 1)
    tester.poke(testcircuit.CE, 1)

    # these check the outputs, as there is a delay between feeding inputs in and getting the results back
    for row in range(num_rows+20):
        for col in range(0,num_cols,2):
            # a necessary adjustment as running tests for multiple clocks after inputting full image
            # to get rest of the outputs
            if row < num_rows:
                tester.poke(testcircuit.I0, image_matrix[row][col])
                tester.poke(testcircuit.I1, image_matrix[row][col+1])
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


def test_downsample_256x256_to_32x32_4px_in_per_clk():
    from .downsample_256x256_to_32x32_4px_in_per_clk import c, downsample_256x256_to_32x32_4px_in_per_clk as testcircuit

    magma.compile("vBuild/" + testcircuit.name, testcircuit, output="verilog",
                  passes=["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst", "flattentypes", "flatten", "verifyconnectivity --noclkrst", "deletedeadinstances"],
                  namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"], context = c)

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.poke(testcircuit.valid_data_in, 1)
    tester.poke(testcircuit.ready_data_out, 1)
    tester.poke(testcircuit.CE, 1)

    # these check the outputs, as there is a delay between feeding inputs in and getting the results back
    for row in range(num_rows+20):
        for col in range(0,num_cols,4):
            # a necessary adjustment as running tests for multiple clocks after inputting full image
            # to get rest of the outputs
            if row < num_rows:
                tester.poke(testcircuit.I0, image_matrix[row][col])
                tester.poke(testcircuit.I1, image_matrix[row][col+1])
                tester.poke(testcircuit.I2, image_matrix[row][col+2])
                tester.poke(testcircuit.I3, image_matrix[row][col+3])
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


def test_downsample_256x256_to_32x32_8px_in_per_clk():
    from .downsample_256x256_to_32x32_8px_in_per_clk import c, downsample_256x256_to_32x32_8px_in_per_clk as testcircuit

    magma.compile("vBuild/" + testcircuit.name, testcircuit, output="verilog",
                  passes=["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst", "flattentypes", "flatten", "verifyconnectivity --noclkrst", "deletedeadinstances"],
                  namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"], context = c)

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.poke(testcircuit.valid_data_in, 1)
    tester.poke(testcircuit.ready_data_out, 1)
    tester.poke(testcircuit.CE, 1)

    # these check the outputs, as there is a delay between feeding inputs in and getting the results back
    for row in range(num_rows+20):
        for col in range(0,num_cols,8):
            # a necessary adjustment as running tests for multiple clocks after inputting full image
            # to get rest of the outputs
            if row < num_rows:
                tester.poke(testcircuit.I0, image_matrix[row][col])
                tester.poke(testcircuit.I1, image_matrix[row][col+1])
                tester.poke(testcircuit.I2, image_matrix[row][col+2])
                tester.poke(testcircuit.I3, image_matrix[row][col+3])
                tester.poke(testcircuit.I4, image_matrix[row][col+4])
                tester.poke(testcircuit.I5, image_matrix[row][col+5])
                tester.poke(testcircuit.I6, image_matrix[row][col+6])
                tester.poke(testcircuit.I7, image_matrix[row][col+7])
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


def test_downsample_256x256_to_32x32_16px_in_per_clk():
    from .downsample_256x256_to_32x32_16px_in_per_clk import c, downsample_256x256_to_32x32_16px_in_per_clk as testcircuit

    magma.compile("vBuild/" + testcircuit.name, testcircuit, output="verilog",
                  passes=["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst", "flattentypes", "flatten", "verifyconnectivity --noclkrst", "deletedeadinstances"],
                  namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"], context = c)

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.poke(testcircuit.valid_data_in, 1)
    tester.poke(testcircuit.ready_data_out, 1)
    tester.poke(testcircuit.CE, 1)

    # these check the outputs, as there is a delay between feeding inputs in and getting the results back
    for row in range(num_rows+20):
        for col in range(0,num_cols,16):
            # a necessary adjustment as running tests for multiple clocks after inputting full image
            # to get rest of the outputs
            if row < num_rows:
                tester.poke(testcircuit.I0, image_matrix[row][col])
                tester.poke(testcircuit.I1, image_matrix[row][col+1])
                tester.poke(testcircuit.I2, image_matrix[row][col+2])
                tester.poke(testcircuit.I3, image_matrix[row][col+3])
                tester.poke(testcircuit.I4, image_matrix[row][col+4])
                tester.poke(testcircuit.I5, image_matrix[row][col+5])
                tester.poke(testcircuit.I6, image_matrix[row][col+6])
                tester.poke(testcircuit.I7, image_matrix[row][col+7])
                tester.poke(testcircuit.I8, image_matrix[row][col+8])
                tester.poke(testcircuit.I9, image_matrix[row][col+9])
                tester.poke(testcircuit.I10, image_matrix[row][col+10])
                tester.poke(testcircuit.I11, image_matrix[row][col+11])
                tester.poke(testcircuit.I12, image_matrix[row][col+12])
                tester.poke(testcircuit.I13, image_matrix[row][col+13])
                tester.poke(testcircuit.I14, image_matrix[row][col+14])
                tester.poke(testcircuit.I15, image_matrix[row][col+15])
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


def test_downsample_256x256_to_32x32_32px_in_per_clk():
    from .downsample_256x256_to_32x32_32px_in_per_clk import c, downsample_256x256_to_32x32_32px_in_per_clk as testcircuit

    magma.compile("vBuild/" + testcircuit.name, testcircuit, output="verilog",
                  passes=["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst", "flattentypes", "flatten", "verifyconnectivity --noclkrst", "deletedeadinstances"],
                  namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"], context = c)

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.poke(testcircuit.valid_data_in, 1)
    tester.poke(testcircuit.ready_data_out, 1)
    tester.poke(testcircuit.CE, 1)

    # these check the outputs, as there is a delay between feeding inputs in and getting the results back
    for row in range(num_rows+20):
        for col in range(0,num_cols,32):
            # a necessary adjustment as running tests for multiple clocks after inputting full image
            # to get rest of the outputs
            if row < num_rows:
                tester.poke(testcircuit.I0, image_matrix[row][col])
                tester.poke(testcircuit.I1, image_matrix[row][col+1])
                tester.poke(testcircuit.I2, image_matrix[row][col+2])
                tester.poke(testcircuit.I3, image_matrix[row][col+3])
                tester.poke(testcircuit.I4, image_matrix[row][col+4])
                tester.poke(testcircuit.I5, image_matrix[row][col+5])
                tester.poke(testcircuit.I6, image_matrix[row][col+6])
                tester.poke(testcircuit.I7, image_matrix[row][col+7])
                tester.poke(testcircuit.I8, image_matrix[row][col+8])
                tester.poke(testcircuit.I9, image_matrix[row][col+9])
                tester.poke(testcircuit.I10, image_matrix[row][col+10])
                tester.poke(testcircuit.I11, image_matrix[row][col+11])
                tester.poke(testcircuit.I12, image_matrix[row][col+12])
                tester.poke(testcircuit.I13, image_matrix[row][col+13])
                tester.poke(testcircuit.I14, image_matrix[row][col+14])
                tester.poke(testcircuit.I15, image_matrix[row][col+15])
                tester.poke(testcircuit.I16, image_matrix[row][col+16])
                tester.poke(testcircuit.I17, image_matrix[row][col+17])
                tester.poke(testcircuit.I18, image_matrix[row][col+18])
                tester.poke(testcircuit.I19, image_matrix[row][col+19])
                tester.poke(testcircuit.I20, image_matrix[row][col+20])
                tester.poke(testcircuit.I21, image_matrix[row][col+21])
                tester.poke(testcircuit.I22, image_matrix[row][col+22])
                tester.poke(testcircuit.I23, image_matrix[row][col+23])
                tester.poke(testcircuit.I24, image_matrix[row][col+24])
                tester.poke(testcircuit.I25, image_matrix[row][col+25])
                tester.poke(testcircuit.I26, image_matrix[row][col+26])
                tester.poke(testcircuit.I27, image_matrix[row][col+27])
                tester.poke(testcircuit.I28, image_matrix[row][col+28])
                tester.poke(testcircuit.I29, image_matrix[row][col+29])
                tester.poke(testcircuit.I30, image_matrix[row][col+30])
                tester.poke(testcircuit.I31, image_matrix[row][col+31])
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



def test_downsample_256x256_to_32x32_64px_in_per_clk():
    from .downsample_256x256_to_32x32_64px_in_per_clk import c, downsample_256x256_to_32x32_64px_in_per_clk as testcircuit

    magma.compile("vBuild/" + testcircuit.name, testcircuit, output="verilog",
                  passes=["rungenerators", "wireclocks-coreir", "verifyconnectivity --noclkrst", "flattentypes", "flatten", "verifyconnectivity --noclkrst", "deletedeadinstances"],
                  namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"], context = c)

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    tester.poke(testcircuit.valid_data_in, 1)
    tester.poke(testcircuit.ready_data_out, 1)
    tester.poke(testcircuit.CE, 1)

    # these check the outputs, as there is a delay between feeding inputs in and getting the results back
    for row in range(num_rows+20):
        for col in range(0,num_cols,64):
            # a necessary adjustment as running tests for multiple clocks after inputting full image
            # to get rest of the outputs
            if row < num_rows:
                tester.poke(testcircuit.I0, image_matrix[row][col])
                tester.poke(testcircuit.I1, image_matrix[row][col+1])
                tester.poke(testcircuit.I2, image_matrix[row][col+2])
                tester.poke(testcircuit.I3, image_matrix[row][col+3])
                tester.poke(testcircuit.I4, image_matrix[row][col+4])
                tester.poke(testcircuit.I5, image_matrix[row][col+5])
                tester.poke(testcircuit.I6, image_matrix[row][col+6])
                tester.poke(testcircuit.I7, image_matrix[row][col+7])
                tester.poke(testcircuit.I8, image_matrix[row][col+8])
                tester.poke(testcircuit.I9, image_matrix[row][col+9])
                tester.poke(testcircuit.I10, image_matrix[row][col+10])
                tester.poke(testcircuit.I11, image_matrix[row][col+11])
                tester.poke(testcircuit.I12, image_matrix[row][col+12])
                tester.poke(testcircuit.I13, image_matrix[row][col+13])
                tester.poke(testcircuit.I14, image_matrix[row][col+14])
                tester.poke(testcircuit.I15, image_matrix[row][col+15])
                tester.poke(testcircuit.I16, image_matrix[row][col+16])
                tester.poke(testcircuit.I17, image_matrix[row][col+17])
                tester.poke(testcircuit.I18, image_matrix[row][col+18])
                tester.poke(testcircuit.I19, image_matrix[row][col+19])
                tester.poke(testcircuit.I20, image_matrix[row][col+20])
                tester.poke(testcircuit.I21, image_matrix[row][col+21])
                tester.poke(testcircuit.I22, image_matrix[row][col+22])
                tester.poke(testcircuit.I23, image_matrix[row][col+23])
                tester.poke(testcircuit.I24, image_matrix[row][col+24])
                tester.poke(testcircuit.I25, image_matrix[row][col+25])
                tester.poke(testcircuit.I26, image_matrix[row][col+26])
                tester.poke(testcircuit.I27, image_matrix[row][col+27])
                tester.poke(testcircuit.I28, image_matrix[row][col+28])
                tester.poke(testcircuit.I29, image_matrix[row][col+29])
                tester.poke(testcircuit.I30, image_matrix[row][col+30])
                tester.poke(testcircuit.I31, image_matrix[row][col+31])
                tester.poke(testcircuit.I32, image_matrix[row][col+32])
                tester.poke(testcircuit.I33, image_matrix[row][col+33])
                tester.poke(testcircuit.I34, image_matrix[row][col+34])
                tester.poke(testcircuit.I35, image_matrix[row][col+35])
                tester.poke(testcircuit.I36, image_matrix[row][col+36])
                tester.poke(testcircuit.I37, image_matrix[row][col+37])
                tester.poke(testcircuit.I38, image_matrix[row][col+38])
                tester.poke(testcircuit.I39, image_matrix[row][col+39])
                tester.poke(testcircuit.I40, image_matrix[row][col+40])
                tester.poke(testcircuit.I41, image_matrix[row][col+41])
                tester.poke(testcircuit.I42, image_matrix[row][col+42])
                tester.poke(testcircuit.I43, image_matrix[row][col+43])
                tester.poke(testcircuit.I44, image_matrix[row][col+44])
                tester.poke(testcircuit.I45, image_matrix[row][col+45])
                tester.poke(testcircuit.I46, image_matrix[row][col+46])
                tester.poke(testcircuit.I47, image_matrix[row][col+47])
                tester.poke(testcircuit.I48, image_matrix[row][col+48])
                tester.poke(testcircuit.I49, image_matrix[row][col+49])
                tester.poke(testcircuit.I50, image_matrix[row][col+50])
                tester.poke(testcircuit.I51, image_matrix[row][col+51])
                tester.poke(testcircuit.I52, image_matrix[row][col+52])
                tester.poke(testcircuit.I53, image_matrix[row][col+53])
                tester.poke(testcircuit.I54, image_matrix[row][col+54])
                tester.poke(testcircuit.I55, image_matrix[row][col+55])
                tester.poke(testcircuit.I56, image_matrix[row][col+56])
                tester.poke(testcircuit.I57, image_matrix[row][col+57])
                tester.poke(testcircuit.I58, image_matrix[row][col+58])
                tester.poke(testcircuit.I59, image_matrix[row][col+59])
                tester.poke(testcircuit.I60, image_matrix[row][col+60])
                tester.poke(testcircuit.I61, image_matrix[row][col+61])
                tester.poke(testcircuit.I62, image_matrix[row][col+62])
                tester.poke(testcircuit.I63, image_matrix[row][col+63])
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
