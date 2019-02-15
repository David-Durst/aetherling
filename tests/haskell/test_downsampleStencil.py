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
num_rows = 16
num_cols = 16
image_matrix = [[row * num_rows + col for col in range(num_cols)] for row in range(num_rows)]
stencil = [[1,2],[3,4]]
# since this is a 2x2 stencil, the right most column and bottom most row are invalid data to be ignored
# these are only emitted so that the rate doesn't have any ugly constants
valid_in_rows = [x for x in range(0,num_rows - 1,2)]
valid_in_cols = [x for x in range(0,num_cols - 1,2)]

def do_convolution_at_point(row, col, input_matrix):
    return ((stencil[0][0] * input_matrix[row][col] + stencil[0][1] * input_matrix[row][col+1] +
    stencil[1][0] * input_matrix[row+1][col] + stencil[1][1] * input_matrix[row+1][col+1]) % 256 // 4)

firstResults = [[do_convolution_at_point(row,col, image_matrix) for col in valid_in_cols] for row in valid_in_rows]
secondResults = [[do_convolution_at_point(row,col, firstResults) for col in valid_in_cols[0:4]] for row in valid_in_rows[0:4]]
thirdResults = [[do_convolution_at_point(row,col, secondResults) for col in valid_in_cols[0:2]] for row in valid_in_rows[0:2]]

valid_out_rows = [x for x in range(len(thirdResults))]
valid_out_cols = [x for x in range(len(thirdResults[0]))]

num_valid_out_rows = len(valid_out_rows)
num_valid_out_cols = len(valid_out_rows)

def test_downsample_stencil_1_per_64():
    from .downsampleStencilChain1Per64 import cirb as downsampleStencilChain1Per64Cirb, downsampleStencilChain1Per64
    scope = Scope()
    sim = CoreIRSimulator(downsampleStencilChain1Per64, downsampleStencilChain1Per64.CLK, context=downsampleStencilChain1Per64Cirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.set_value(downsampleStencilChain1Per64.valid_data_in, 1, scope)
    sim.set_value(downsampleStencilChain1Per64.ready_data_out, 1, scope)
    sim.set_value(downsampleStencilChain1Per64.CE, 1, scope)

    # these check the outputs, as there is a delay between feeding inputs in and getting the results back
    cur_row_to_check = 0
    cur_col_to_check = 0
    numLb0Valid = 0
    numLb1Valid = 0
    numLb2Valid = 0
    lb1EverBeenValid = False
    lb2EverBeenValid = False
    successfully_checked_all_valid_outputs = False
    def print_all_helpers():
        print("lb0Ready: " + str(sim.get_value(downsampleStencilChain1Per64.lb0Ready, scope)))
        print("lb0Valid: " + str(sim.get_value(downsampleStencilChain1Per64.lb0Valid, scope)))
        print("lb0CE: " + str(sim.get_value(downsampleStencilChain1Per64.lb0CE, scope)))
        print("lb1Ready: " + str(sim.get_value(downsampleStencilChain1Per64.lb1Ready, scope)))
        print("lb1Valid: " + str(sim.get_value(downsampleStencilChain1Per64.lb1Valid, scope)))
        print("lb1CE: " + str(sim.get_value(downsampleStencilChain1Per64.lb1CE, scope)))
        print("lb2Ready: " + str(sim.get_value(downsampleStencilChain1Per64.lb2Ready, scope)))
        print("lb2Valid: " + str(sim.get_value(downsampleStencilChain1Per64.lb2Valid, scope)))
        print("lb2CE: " + str(sim.get_value(downsampleStencilChain1Per64.lb2CE, scope)))
        print("")
        print("lb0Out: " + str([seq2int(sim.get_value(downsampleStencilChain1Per64.lb0Out[0][r][c], scope)) for r in range(2) for c in range(2)]))
        print("lb1In: " + str(seq2int(sim.get_value(downsampleStencilChain1Per64.lb1In[0][0], scope))))
        print("lb1Out: " + str([seq2int(sim.get_value(downsampleStencilChain1Per64.lb1Out[0][r][c], scope)) for r in range(2) for c in range(2)]))
        print("lb2In: " + str(seq2int(sim.get_value(downsampleStencilChain1Per64.lb2In[0][0], scope))))
        print("O0: " + str(seq2int(sim.get_value(downsampleStencilChain1Per64.O0, scope))))
    for row in range(num_rows+6):
        for col in range(num_cols):
            # a necessary adjustment as running tests for multiple clocks after inputting full image
            # to get rest of the outputs
            if row < num_rows:
                sim.set_value(downsampleStencilChain1Per64.I0, int2seq(image_matrix[row][col], int_width))
            sim.evaluate()
            assert sim.get_value(downsampleStencilChain1Per64.ready_data_in, scope) == 1
            #print("starting row {}, col {}".format(row, col))
            if False:
                if sim.get_value(downsampleStencilChain1Per64.lb0Valid, scope) == 1 and \
                        sim.get_value(downsampleStencilChain1Per64.lb1Valid, scope) == 0 and \
                        sim.get_value(downsampleStencilChain1Per64.lb2Valid, scope) == 0:
                    print("lb 0 is valid, happened {} times before".format(numLb0Valid))
                    #print_all_helpers()
                elif sim.get_value(downsampleStencilChain1Per64.lb1Valid, scope) == 1 and \
                    sim.get_value(downsampleStencilChain1Per64.lb2Valid, scope) == 0:
                    print("lb 1 is valid, happened {} times before".format(numLb1Valid))
                    #print_all_helpers()
                    lb1EverBeenValid = True
                elif sim.get_value(downsampleStencilChain1Per64.lb2Valid, scope) == 1:
                    print("lb 2 is valid, happened {} times before".format(numLb2Valid))
                    print("row {}, col {}".format(row, col))
                    #print_all_helpers()
                    lb2EverBeenValid = True
            #else:
            #    print_all_helpers()
            #elif lb1EverBeenValid is False and row > 3:
            #    print("lb1 not going valid at right time")
            #elif lb2EverBeenValid is False and row > 7:
            #    print("lb2 not going valid at right time")
                if sim.get_value(downsampleStencilChain1Per64.lb0Valid, scope) == 1:
                    numLb0Valid += 1
                if sim.get_value(downsampleStencilChain1Per64.lb1Valid, scope) == 1:
                    numLb1Valid += 1
                if sim.get_value(downsampleStencilChain1Per64.lb2Valid, scope) == 1:
                    numLb2Valid += 1
            if sim.get_value(downsampleStencilChain1Per64.valid_data_out, scope) == 1:
                if cur_row_to_check in valid_out_rows and cur_col_to_check in valid_out_cols:
                    if seq2int(sim.get_value(downsampleStencilChain1Per64.O0, scope)) != thirdResults[cur_row_to_check][cur_col_to_check]:
                        print(cur_col_to_check)
                    #print("Overall row and col : ({}, {})".format(row, col))
                    #print("Current output row and col : ({}, {})".format(cur_row_to_check, cur_col_to_check))
                    assert seq2int(sim.get_value(downsampleStencilChain1Per64.O0, scope)) == thirdResults[cur_row_to_check][cur_col_to_check]
                cur_col_to_check += 1
                cur_col_to_check = cur_col_to_check % num_valid_out_cols
                if cur_col_to_check == 0:
                    cur_row_to_check += 1
                    cur_row_to_check = cur_row_to_check
                if not cur_row_to_check in valid_out_rows:
                    successfully_checked_all_valid_outputs = True
                    break
            sim.advance_cycle()
            #print("done row {}, col {}".format(row, col))
            #print("")
        if successfully_checked_all_valid_outputs:
            break
    assert successfully_checked_all_valid_outputs


def test_downsample_stencil_1_per_32():
    from .downsampleStencilChain1Per32 import cirb as downsampleStencilChain1Per32Cirb, downsampleStencilChain1Per32
    scope = Scope()
    sim = CoreIRSimulator(downsampleStencilChain1Per32, downsampleStencilChain1Per32.CLK, context=downsampleStencilChain1Per32Cirb.context,
                          namespaces=["aetherlinglib", "commonlib", "mantle", "coreir", "global"])

    sim.set_value(downsampleStencilChain1Per32.valid_data_in, 1, scope)
    sim.set_value(downsampleStencilChain1Per32.ready_data_out, 1, scope)
    sim.set_value(downsampleStencilChain1Per32.CE, 1, scope)

    # these check the outputs, as there is a delay between feeding inputs in and getting the results back
    cur_row_to_check = 0
    cur_col_to_check = 0
    successfully_checked_all_valid_outputs = False
    for row in range(num_rows+6):
        for col in range(0,num_cols,2):
            # a necessary adjustment as running tests for multiple clocks after inputting full image
            # to get rest of the outputs
            if row < num_rows:
                sim.set_value(downsampleStencilChain1Per32.I0, int2seq(image_matrix[row][col], int_width))
                sim.set_value(downsampleStencilChain1Per32.I1, int2seq(image_matrix[row][col+1], int_width))
            sim.evaluate()
            assert sim.get_value(downsampleStencilChain1Per32.ready_data_in, scope) == 1
            if sim.get_value(downsampleStencilChain1Per32.valid_data_out, scope) == 1:
                if cur_row_to_check in valid_out_rows and cur_col_to_check in valid_out_cols:
                    if seq2int(sim.get_value(downsampleStencilChain1Per32.O0, scope)) != thirdResults[cur_row_to_check][cur_col_to_check]:
                        print(cur_col_to_check)
                    print("Overall row and col : ({}, {})".format(row, col))
                    assert seq2int(sim.get_value(downsampleStencilChain1Per32.O0, scope)) == thirdResults[cur_row_to_check][cur_col_to_check]
                cur_col_to_check += 1
                cur_col_to_check = cur_col_to_check % num_valid_out_cols
                if cur_col_to_check == 0:
                    cur_row_to_check += 1
                    cur_row_to_check = cur_row_to_check
                if not cur_row_to_check in valid_out_rows:
                    successfully_checked_all_valid_outputs = True
                    break
            sim.advance_cycle()
        if successfully_checked_all_valid_outputs:
            break
    assert successfully_checked_all_valid_outputs
