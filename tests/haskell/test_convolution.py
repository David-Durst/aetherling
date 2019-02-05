from aetherling.modules.reduce import ReduceSequential, ReduceParallel, ReducePartiallyParallel, renameCircuitForReduce
from magma.backend.coreir_ import CoreIRBackend
from magma.bitutils import *
from coreir.context import *
from magma.simulator.coreir_simulator import CoreIRSimulator
import coreir
from magma.scope import Scope
from mantle.coreir.arith import *
from mantle.coreir import DefineCoreirConst

int_width = 8
# this computes the convlution that is computed at different throughputs by the
# differently scheduled Aetherling pipelines
num_rows = 8
num_cols = 8
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

def test_partial_parallel_2_convolution():
    from .partialParallel2Convolution import cirb as partialParallel2ConvolutionCirb, partialParallel2Convolution
    scope = Scope()
    sim = CoreIRSimulator(partialParallel2Convolution, partialParallel2Convolution.CLK, context=partialParallel2ConvolutionCirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.set_value(partialParallel2Convolution.valid_data_in, 1, scope)
    sim.set_value(partialParallel2Convolution.ready_data_out, 1, scope)
    sim.set_value(partialParallel2Convolution.CE, 1, scope)

    # these check the outputs, as there is a delay between feeding inputs in and getting the results back
    cur_row_to_check = 0
    cur_col_to_check = 0
    successfully_checked_all_valid_outputs = False
    for row in range(num_rows+3):
        for col in range(0,num_cols,2):
            # a necessary adjustment as running tests for multiple clocks after inputting full image
            # to get rest of the outputs
            if row < num_rows:
                sim.set_value(partialParallel2Convolution.I0, int2seq(image_matrix[row][col], int_width))
                sim.set_value(partialParallel2Convolution.I1, int2seq(image_matrix[row][col+1], int_width))
            sim.evaluate()
            assert sim.get_value(partialParallel2Convolution.ready_data_in, scope) == 1
            if sim.get_value(partialParallel2Convolution.valid_data_out, scope) == 1:
                if cur_row_to_check in valid_rows and cur_col_to_check in valid_cols:
                    if seq2int(sim.get_value(partialParallel2Convolution.O0, scope)) != results[cur_row_to_check][cur_col_to_check]:
                        print(cur_col_to_check)
                    assert seq2int(sim.get_value(partialParallel2Convolution.O0, scope)) == results[cur_row_to_check][cur_col_to_check]
                    if cur_col_to_check + 1 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel2Convolution.O1, scope)) == results[cur_row_to_check][cur_col_to_check+1]
                if not cur_row_to_check in valid_rows:
                    successfully_checked_all_valid_outputs = True
                    break
                cur_col_to_check += 2
                cur_col_to_check = cur_col_to_check % num_cols
                if cur_col_to_check == 0:
                    cur_row_to_check += 1
                    cur_row_to_check = cur_row_to_check % num_rows
            sim.advance_cycle()
        if successfully_checked_all_valid_outputs:
            break
    assert successfully_checked_all_valid_outputs

def test_partial_parallel_4_convolution():
    from .partialParallel4Convolution import cirb as partialParallel4ConvolutionCirb, partialParallel4Convolution
    scope = Scope()
    sim = CoreIRSimulator(partialParallel4Convolution, partialParallel4Convolution.CLK, context=partialParallel4ConvolutionCirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.set_value(partialParallel4Convolution.valid_data_in, 1, scope)
    sim.set_value(partialParallel4Convolution.ready_data_out, 1, scope)
    sim.set_value(partialParallel4Convolution.CE, 1, scope)

    # these check the outputs, as there is a delay between feeding inputs in and getting the results back
    cur_row_to_check = 0
    cur_col_to_check = 0
    successfully_checked_all_valid_outputs = False
    for row in range(num_rows+3):
        for col in range(0,num_cols,4):
            # a necessary adjustment as running tests for multiple clocks after inputting full image
            # to get rest of the outputs
            if row < num_rows:
                sim.set_value(partialParallel4Convolution.I0, int2seq(image_matrix[row][col], int_width))
                sim.set_value(partialParallel4Convolution.I1, int2seq(image_matrix[row][col+1], int_width))
                sim.set_value(partialParallel4Convolution.I2, int2seq(image_matrix[row][col+2], int_width))
                sim.set_value(partialParallel4Convolution.I3, int2seq(image_matrix[row][col+3], int_width))
            sim.evaluate()
            assert sim.get_value(partialParallel4Convolution.ready_data_in, scope) == 1
            if sim.get_value(partialParallel4Convolution.valid_data_out, scope) == 1:
                if cur_row_to_check in valid_rows and cur_col_to_check in valid_cols:
                    if seq2int(sim.get_value(partialParallel4Convolution.O0, scope)) != results[cur_row_to_check][cur_col_to_check]:
                        print(cur_col_to_check)
                    assert seq2int(sim.get_value(partialParallel4Convolution.O0, scope)) == results[cur_row_to_check][cur_col_to_check]
                    if cur_col_to_check + 1 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel4Convolution.O1, scope)) == results[cur_row_to_check][cur_col_to_check+1]
                    if cur_col_to_check + 2 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel4Convolution.O2, scope)) == results[cur_row_to_check][cur_col_to_check+2]
                    if cur_col_to_check + 3 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel4Convolution.O3, scope)) == results[cur_row_to_check][cur_col_to_check+3]
                if not cur_row_to_check in valid_rows:
                    successfully_checked_all_valid_outputs = True
                    break
                cur_col_to_check += 4
                cur_col_to_check = cur_col_to_check % num_cols
                if cur_col_to_check == 0:
                    cur_row_to_check += 1
                    cur_row_to_check = cur_row_to_check % num_rows
            sim.advance_cycle()
        if successfully_checked_all_valid_outputs:
            break
    assert successfully_checked_all_valid_outputs

def test_partial_parallel_8_convolution():
    from .partialParallel8Convolution import cirb as partialParallel8ConvolutionCirb, partialParallel8Convolution
    scope = Scope()
    sim = CoreIRSimulator(partialParallel8Convolution, partialParallel8Convolution.CLK, context=partialParallel8ConvolutionCirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.set_value(partialParallel8Convolution.valid_data_in, 1, scope)
    sim.set_value(partialParallel8Convolution.ready_data_out, 1, scope)
    sim.set_value(partialParallel8Convolution.CE, 1, scope)

    # these check the outputs, as there is a delay between feeding inputs in and getting the results back
    cur_row_to_check = 0
    cur_col_to_check = 0
    successfully_checked_all_valid_outputs = False
    for row in range(num_rows+3):
        for col in range(0,num_cols,8):
            # a necessary adjustment as running tests for multiple clocks after inputting full image
            # to get rest of the outputs
            if row < num_rows:
                sim.set_value(partialParallel8Convolution.I0, int2seq(image_matrix[row][col], int_width))
                sim.set_value(partialParallel8Convolution.I1, int2seq(image_matrix[row][col+1], int_width))
                sim.set_value(partialParallel8Convolution.I2, int2seq(image_matrix[row][col+2], int_width))
                sim.set_value(partialParallel8Convolution.I3, int2seq(image_matrix[row][col+3], int_width))
                sim.set_value(partialParallel8Convolution.I4, int2seq(image_matrix[row][col+4], int_width))
                sim.set_value(partialParallel8Convolution.I5, int2seq(image_matrix[row][col+5], int_width))
                sim.set_value(partialParallel8Convolution.I6, int2seq(image_matrix[row][col+6], int_width))
                sim.set_value(partialParallel8Convolution.I7, int2seq(image_matrix[row][col+7], int_width))
            sim.evaluate()
            assert sim.get_value(partialParallel8Convolution.ready_data_in, scope) == 1
            if sim.get_value(partialParallel8Convolution.valid_data_out, scope) == 1:
                if cur_row_to_check in valid_rows and cur_col_to_check in valid_cols:
                    if seq2int(sim.get_value(partialParallel8Convolution.O0, scope)) != results[cur_row_to_check][cur_col_to_check]:
                        print(cur_col_to_check)
                    assert seq2int(sim.get_value(partialParallel8Convolution.O0, scope)) == results[cur_row_to_check][cur_col_to_check]
                    if cur_col_to_check + 1 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel8Convolution.O1, scope)) == results[cur_row_to_check][cur_col_to_check+1]
                    if cur_col_to_check + 2 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel8Convolution.O2, scope)) == results[cur_row_to_check][cur_col_to_check+2]
                    if cur_col_to_check + 3 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel8Convolution.O3, scope)) == results[cur_row_to_check][cur_col_to_check+3]
                    if cur_col_to_check + 4 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel8Convolution.O4, scope)) == results[cur_row_to_check][cur_col_to_check+4]
                    if cur_col_to_check + 5 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel8Convolution.O5, scope)) == results[cur_row_to_check][cur_col_to_check+5]
                    if cur_col_to_check + 6 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel8Convolution.O6, scope)) == results[cur_row_to_check][cur_col_to_check+6]
                    if cur_col_to_check + 7 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel8Convolution.O7, scope)) == results[cur_row_to_check][cur_col_to_check+7]
                if not cur_row_to_check in valid_rows:
                    successfully_checked_all_valid_outputs = True
                    break
                cur_col_to_check += 8
                cur_col_to_check = cur_col_to_check % num_cols
                if cur_col_to_check == 0:
                    cur_row_to_check += 1
                    cur_row_to_check = cur_row_to_check % num_rows
            sim.advance_cycle()
        if successfully_checked_all_valid_outputs:
            break
    assert successfully_checked_all_valid_outputs


def test_partial_parallel_16_convolution():
    from .partialParallel16Convolution import cirb as partialParallel16ConvolutionCirb, partialParallel16Convolution
    scope = Scope()
    sim = CoreIRSimulator(partialParallel16Convolution, partialParallel16Convolution.CLK, context=partialParallel16ConvolutionCirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.set_value(partialParallel16Convolution.valid_data_in, 1, scope)
    sim.set_value(partialParallel16Convolution.ready_data_out, 1, scope)
    sim.set_value(partialParallel16Convolution.CE, 1, scope)

    # these check the outputs, as there is a delay between feeding inputs in and getting the results back
    cur_row_to_check = 0
    cur_col_to_check = 0
    successfully_checked_all_valid_outputs = False
    for row in range(0, num_rows+6, 2):
        for col in range(0,num_cols,8):
            # a necessary adjustment as running tests for multiple clocks after inputting full image
            # to get rest of the outputs
            if row < num_rows:
                sim.set_value(partialParallel16Convolution.I0, int2seq(image_matrix[row][col], int_width))
                sim.set_value(partialParallel16Convolution.I1, int2seq(image_matrix[row][col+1], int_width))
                sim.set_value(partialParallel16Convolution.I2, int2seq(image_matrix[row][col+2], int_width))
                sim.set_value(partialParallel16Convolution.I3, int2seq(image_matrix[row][col+3], int_width))
                sim.set_value(partialParallel16Convolution.I4, int2seq(image_matrix[row][col+4], int_width))
                sim.set_value(partialParallel16Convolution.I5, int2seq(image_matrix[row][col+5], int_width))
                sim.set_value(partialParallel16Convolution.I6, int2seq(image_matrix[row][col+6], int_width))
                sim.set_value(partialParallel16Convolution.I7, int2seq(image_matrix[row][col+7], int_width))
                sim.set_value(partialParallel16Convolution.I8, int2seq(image_matrix[row+1][col], int_width))
                sim.set_value(partialParallel16Convolution.I9, int2seq(image_matrix[row+1][col+1], int_width))
                sim.set_value(partialParallel16Convolution.I10, int2seq(image_matrix[row+1][col+2], int_width))
                sim.set_value(partialParallel16Convolution.I11, int2seq(image_matrix[row+1][col+3], int_width))
                sim.set_value(partialParallel16Convolution.I12, int2seq(image_matrix[row+1][col+4], int_width))
                sim.set_value(partialParallel16Convolution.I13, int2seq(image_matrix[row+1][col+5], int_width))
                sim.set_value(partialParallel16Convolution.I14, int2seq(image_matrix[row+1][col+6], int_width))
                sim.set_value(partialParallel16Convolution.I15, int2seq(image_matrix[row+1][col+7], int_width))
            sim.evaluate()
            assert sim.get_value(partialParallel16Convolution.ready_data_in, scope) == 1
            if sim.get_value(partialParallel16Convolution.valid_data_out, scope) == 1:
                if cur_row_to_check in valid_rows and cur_col_to_check in valid_cols:
                    if seq2int(sim.get_value(partialParallel16Convolution.O0, scope)) != results[cur_row_to_check][cur_col_to_check]:
                        print(cur_col_to_check)
                    assert seq2int(sim.get_value(partialParallel16Convolution.O0, scope)) == results[cur_row_to_check][cur_col_to_check]
                    if cur_col_to_check + 1 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel16Convolution.O1, scope)) == results[cur_row_to_check][cur_col_to_check+1]
                    if cur_col_to_check + 2 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel16Convolution.O2, scope)) == results[cur_row_to_check][cur_col_to_check+2]
                    if cur_col_to_check + 3 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel16Convolution.O3, scope)) == results[cur_row_to_check][cur_col_to_check+3]
                    if cur_col_to_check + 4 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel16Convolution.O4, scope)) == results[cur_row_to_check][cur_col_to_check+4]
                    if cur_col_to_check + 5 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel16Convolution.O5, scope)) == results[cur_row_to_check][cur_col_to_check+5]
                    if cur_col_to_check + 6 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel16Convolution.O6, scope)) == results[cur_row_to_check][cur_col_to_check+6]
                    if cur_col_to_check + 7 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel16Convolution.O7, scope)) == results[cur_row_to_check][cur_col_to_check+7]
                    if cur_row_to_check + 1 in valid_rows and cur_col_to_check in valid_cols:
                        assert seq2int(sim.get_value(partialParallel16Convolution.O8, scope)) == results[cur_row_to_check+1][cur_col_to_check]
                    if cur_row_to_check + 1 in valid_rows and cur_col_to_check + 1 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel16Convolution.O9, scope)) == results[cur_row_to_check+1][cur_col_to_check+1]
                    if cur_row_to_check + 1 in valid_rows and cur_col_to_check + 2 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel16Convolution.O10, scope)) == results[cur_row_to_check+1][cur_col_to_check+2]
                    if cur_row_to_check + 1 in valid_rows and cur_col_to_check + 3 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel16Convolution.O11, scope)) == results[cur_row_to_check+1][cur_col_to_check+3]
                    if cur_row_to_check + 1 in valid_rows and cur_col_to_check + 4 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel16Convolution.O12, scope)) == results[cur_row_to_check+1][cur_col_to_check+4]
                    if cur_row_to_check + 1 in valid_rows and cur_col_to_check + 5 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel16Convolution.O13, scope)) == results[cur_row_to_check+1][cur_col_to_check+5]
                    if cur_row_to_check + 1 in valid_rows and cur_col_to_check + 6 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel16Convolution.O14, scope)) == results[cur_row_to_check+1][cur_col_to_check+6]
                    if cur_row_to_check + 1 in valid_rows and cur_col_to_check + 7 in valid_cols:
                        assert seq2int(sim.get_value(partialParallel16Convolution.O15, scope)) == results[cur_row_to_check+1][cur_col_to_check+7]
                if not cur_row_to_check in valid_rows or not cur_row_to_check + 1 in valid_rows:
                    successfully_checked_all_valid_outputs = True
                    break
                cur_col_to_check += 8
                cur_col_to_check = cur_col_to_check % num_cols
                if cur_col_to_check == 0:
                    cur_row_to_check += 2
                    cur_row_to_check = cur_row_to_check % num_rows
            sim.advance_cycle()
        if successfully_checked_all_valid_outputs:
            break
    assert successfully_checked_all_valid_outputs

