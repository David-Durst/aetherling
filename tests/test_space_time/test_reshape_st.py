from aetherling.modules.permutation.assign_banks import *
from aetherling.modules.permutation import build_permutation_graph
from aetherling.space_time.type_helpers import get_shared_and_diff_subtypes
from aetherling.space_time.reshape_st import DefineReshape_ST
from aetherling.helpers.fault_helpers import compile_and_run, compile
import fault

def test_2_3_flip_reshape():
    t_len = 3
    s_len = 2
    input_type = ST_SSeq(s_len, ST_TSeq(t_len, 0, ST_Int()))
    output_type = ST_TSeq(t_len, 0, ST_SSeq(s_len, ST_Int()))
    graph = build_permutation_graph(input_type, output_type)
    testcircuit = DefineReshape_ST(input_type, output_type)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    check_reshape(graph, 2, testcircuit.output_delay, tester, 0, 0)

def test_2_3_shared_tseq_2_0_flip_reshape():
    no = 3
    io = 0
    ni = 2
    nii = 2
    iii = 0
    input_type = ST_TSeq(no, io, ST_SSeq(ni, ST_TSeq(nii, iii, ST_Int())))
    output_type = ST_SSeq(ni, ST_TSeq(no, io, ST_TSeq(nii, iii, ST_Int())))
    graph = build_permutation_graph(input_type, output_type)
    testcircuit = DefineReshape_ST(input_type, output_type)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    check_reshape(graph, 2, testcircuit.output_delay, tester, 0, 0)

def test_2_3_shared_sseq_2_flip_reshape():
    no = 3
    io = 0
    ni = 2
    nii = 2
    input_type = ST_TSeq(no, io, ST_SSeq(ni, ST_SSeq(nii, ST_Int())))
    output_type = ST_SSeq(ni, ST_TSeq(no, io, ST_SSeq(nii, ST_Int())))
    graph = build_permutation_graph(input_type, output_type)
    testcircuit = DefineReshape_ST(input_type, output_type)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    check_reshape(graph, 2, testcircuit.output_delay, tester, 1, 1)

def test_sseq_2_tseq_2_to_sseq_4_tseq_1_reshape():
    input_type = ST_SSeq(2, ST_TSeq(2, 0, ST_Int()))
    output_type = ST_SSeq(4, ST_TSeq(1, 1, ST_Int()))
    graph = build_permutation_graph(input_type, output_type)
    testcircuit = DefineReshape_ST(input_type, output_type)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    check_reshape(graph, 2, testcircuit.output_delay, tester, 0, 0)

def test_2_3_shared_sseq_2_tseq_3_3_flip_reshape():
    no = 3
    io = 0
    ni = 2
    nii = 2
    niii = 3
    iiii = 3
    input_type = ST_TSeq(no, io, ST_SSeq(ni, ST_SSeq(nii, ST_TSeq(niii, iiii, ST_Int()))))
    output_type = ST_SSeq(ni, ST_TSeq(no, io, ST_SSeq(nii, ST_TSeq(niii, iiii, ST_Int()))))
    graph = build_permutation_graph(input_type, output_type)
    testcircuit = DefineReshape_ST(input_type, output_type)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    check_reshape(graph, 2, testcircuit.output_delay, tester, 1, 1)

def check_reshape(graph: InputOutputGraph, num_t, delay, tester, num_flattens_in, num_flattens_out, has_ce = False, has_reset = False):
    clocks = len(graph.input_nodes)
    if has_ce:
        tester.circuit.CE = True
    if has_reset:
        tester.circuit.reset = False
    clk = 0
    input_element = -1
    output_element = -1

    in_ports = tester.circuit.I
    for i in range(num_flattens_in):
        in_ports = flatten(in_ports)

    out_ports = tester.circuit.O
    for i in range(num_flattens_out):
        out_ports = flatten(out_ports)

    for i in range(num_t * clocks + delay):
        input_element = (input_element + 1) % clocks
        if input_element >= delay or output_element != -1:
            output_element = (output_element + 1) % clocks
        tester.print("clk: {}\n".format(clk))

        for k in range(len(graph.input_nodes[input_element].flat_idxs)):
            if not graph.input_nodes[input_element].flat_idxs[k].invalid:
                tester.poke(in_ports[k].port, graph.input_nodes[input_element].flat_idxs[k].idx)
                tester.print("input {}: %d\n".format(k), in_ports[k])

        tester.eval()

#        for k in range(len(graph.input_nodes[input_element].flat_idxs)):
#            tester.print("ram_wr {}: %d\n".format(k), tester.circuit.ram_wr[k])
#            tester.print("addr_wr {}: %d\n".format(k), tester.circuit.addr_wr[k])

#        for k in range(len(graph.output_nodes[output_element].flat_idxs)):
#            tester.print("ram_rd {}: %d\n".format(k), tester.circuit.ram_rd[k])
#            tester.print("addr_rd {}: %d\n".format(k), tester.circuit.addr_rd[k])

        if i >= delay:
            for k in range(len(graph.output_nodes[output_element].flat_idxs)):
                tester.print("output {}: %d\n".format(k), out_ports[k])
            for k in range(len(graph.output_nodes[output_element].flat_idxs)):
                if not graph.output_nodes[output_element].flat_idxs[k].invalid:
                    out_ports[k].expect(graph.output_nodes[output_element].flat_idxs[k].idx)

#        tester.print("reshape write counter: %d\n", tester.circuit.reshape_write_counter)
#        tester.print("ram first valid write: %d\n \n", tester.circuit.first_valid)
        tester.step(2)
        clk += 1
    compile_and_run(tester)
