from aetherling.modules.permutation.assign_banks import *
from aetherling.modules.permutation import build_permutation_graph
from aetherling.space_time import flatten
from aetherling.space_time.reshape_st import DefineReshape_ST
from aetherling.helpers.fault_helpers import compile_and_run
import fault

def test_2_3_flip_reshape():
    """
    Tests the most basic flip
    """
    t_len = 3
    s_len = 2
    input_type = ST_SSeq(s_len, ST_TSeq(t_len, 0, ST_Int()))
    output_type = ST_TSeq(t_len, 0, ST_SSeq(s_len, ST_Int()))
    graph = build_permutation_graph(input_type, output_type)
    testcircuit = DefineReshape_ST(input_type, output_type)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    check_reshape(graph, 2, testcircuit.output_delay, tester, 0, 0)

def test_2_2_3_flip_reshape():
    """
    Tests the flip where moving two sseqs at the same time
    """
    t_len = 3
    s_len_0 = 2
    s_len_1 = 2
    input_type = ST_SSeq(s_len_0, ST_SSeq(s_len_1, ST_TSeq(t_len, 0, ST_Int())))
    output_type = ST_TSeq(t_len, 0, ST_SSeq(s_len_0, ST_SSeq(s_len_1, ST_Int())))
    graph = build_permutation_graph(input_type, output_type)
    testcircuit = DefineReshape_ST(input_type, output_type)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    check_reshape(graph, 2, testcircuit.output_delay, tester, 1, 1)

def test_shared_tseq_2_0_2_3_flip_reshape():
    """
    Tests flip with a shared tseq on the outside
    """
    no = 2
    io = 0
    ni = 3
    ii = 0
    nii = 2
    input_type = ST_TSeq(no, io, ST_TSeq(ni, ii, ST_SSeq(nii, ST_Int())))
    output_type = ST_TSeq(no, io, ST_SSeq(nii, ST_TSeq(ni, ii, ST_Int())))
    graph = build_permutation_graph(input_type, output_type)
    testcircuit = DefineReshape_ST(input_type, output_type)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    check_reshape(graph, 2, testcircuit.output_delay, tester, 0, 0)

def test_shared_sseq_2_2_3_flip_reshape():
    """
    Tests flip with a shared sseq on the outside
    """
    no = 2
    ni = 3
    ii = 0
    nii = 2
    input_type = ST_SSeq(no, ST_TSeq(ni, ii, ST_SSeq(nii, ST_Int())))
    output_type = ST_SSeq(no, ST_SSeq(nii, ST_TSeq(ni, ii, ST_Int())))
    graph = build_permutation_graph(input_type, output_type)
    testcircuit = DefineReshape_ST(input_type, output_type)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    check_reshape(graph, 2, testcircuit.output_delay, tester, 1, 1)

def test_shared_sseq_2_sseq_6_diff_2_3_flip_reshape():
    """
    Tests flip with multiple shared sseq on the outside
    """
    no = 2
    ni = 3
    ii = 0
    nii = 2
    input_type = ST_SSeq(no, ST_SSeq(6, ST_TSeq(ni, ii, ST_SSeq(nii, ST_Int()))))
    output_type = ST_SSeq(no, ST_SSeq(6, ST_SSeq(nii, ST_TSeq(ni, ii, ST_Int()))))
    graph = build_permutation_graph(input_type, output_type)
    testcircuit = DefineReshape_ST(input_type, output_type)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    check_reshape(graph, 2, testcircuit.output_delay, tester, 2, 2)

def test_shared_sseq_2_tseq_3_1_diff_2_3_flip_reshape():
    """
    Tests flip with shared sseq and tseq on the outside
    """
    no = 2
    ni = 3
    ii = 0
    nii = 2
    input_type = ST_SSeq(no, ST_TSeq(3, 1, ST_TSeq(ni, ii, ST_SSeq(nii, ST_Int()))))
    output_type = ST_SSeq(no, ST_TSeq(3, 1, ST_SSeq(nii, ST_TSeq(ni, ii, ST_Int()))))
    graph = build_permutation_graph(input_type, output_type)
    testcircuit = DefineReshape_ST(input_type, output_type)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    check_reshape(graph, 2, testcircuit.output_delay, tester, 1, 1)

def test_2_3_shared_tseq_2_0_flip_reshape():
    """
    Tests flip with a shared tseq on the inside
    """
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
    """
    Tests flip with a shared sseq on the inside
    """
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

def test_2_3_shared_sseq_2_tseq_3_3_flip_reshape():
    """
    Tests reshape with both sseq and tseq on inside
    """
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

def test_shared_outer_sseq_2_tseq_3_1_diff_2_3_shared_inner_sseq_2_tseq_3_3_flip_reshape():
    """
    Tests reshape with both sseq and tseq on inside and outside
    """
    no = 3
    io = 0
    ni = 2
    nii = 2
    niii = 3
    iiii = 3
    input_type = ST_SSeq(2, ST_TSeq(3, 1, ST_TSeq(no, io, ST_SSeq(ni, ST_SSeq(nii, ST_TSeq(niii, iiii, ST_Int()))))))
    output_type = ST_SSeq(2, ST_TSeq(3, 1, ST_SSeq(ni, ST_TSeq(no, io, ST_SSeq(nii, ST_TSeq(niii, iiii, ST_Int()))))))
    graph = build_permutation_graph(input_type, output_type)
    testcircuit = DefineReshape_ST(input_type, output_type)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    check_reshape(graph, 2, testcircuit.output_delay, tester, 2, 2)

def test_sseq_2_tseq_2_to_sseq_4_tseq_1_reshape():
    """
    Tests reshape with different input and output ports
    """
    input_type = ST_SSeq(2, ST_TSeq(2, 0, ST_Int()))
    output_type = ST_SSeq(4, ST_TSeq(1, 1, ST_Int()))
    graph = build_permutation_graph(input_type, output_type)
    testcircuit = DefineReshape_ST(input_type, output_type)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    check_reshape(graph, 2, testcircuit.output_delay, tester, 0, 0)

def test_tseq_2_0_tseq_1_2_sseq_1_to_tseq_1_5_sseq_2():
    input_type = ST_TSeq(2, 0, ST_TSeq(1, 2, ST_SSeq(1, ST_Int())))
    output_type = ST_TSeq(1, 5, ST_SSeq(2, ST_Int()))
    graph = build_permutation_graph(input_type, output_type)
    testcircuit = DefineReshape_ST(input_type, output_type)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    check_reshape(graph, 2, testcircuit.output_delay, tester, 0, 0)

def test_tseq_2_0_tseq_3_2_sseq_1_to_tseq_2_8_sseq_3():
    input_type = ST_TSeq(2, 0, ST_TSeq(3, 2, ST_SSeq(1, ST_Int())))
    output_type = ST_TSeq(2, 8, ST_SSeq(3, ST_Int()))
    graph = build_permutation_graph(input_type, output_type)
    testcircuit = DefineReshape_ST(input_type, output_type)
    tester = fault.Tester(testcircuit, testcircuit.CLK)
    check_reshape(graph, 2, testcircuit.output_delay, tester, 0, 0)

def test_same():
    DefineReshape_ST(ST_TSeq(2, 0, ST_SSeq(3, ST_Int())), ST_TSeq(2, 0, ST_SSeq(3, ST_Int())), has_valid=True)()

def test_same_modulo_nesting():
    input_type = ST_TSeq(2, 0, ST_SSeq(1, ST_SSeq(3, ST_Int())))
    output_type = ST_SSeq(1, ST_TSeq(2, 0, ST_SSeq(3, ST_Int())))
    DefineReshape_ST(input_type, output_type)

def test_tseq_2_0_sseq_50_stuple_3_to_tseq_2_0_sseq_50_sseq_3():
    input_type = ST_TSeq(2, 0, ST_SSeq(50, ST_SSeq_Tuple(3, ST_Int())))
    output_type = ST_TSeq(2, 0, ST_SSeq(50, ST_SSeq(3, ST_Int())))
    graph = build_permutation_graph(input_type, output_type)
    testcircuit = DefineReshape_ST(input_type, output_type, has_valid=True)
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

    in_iterations = -1
    out_iterations = -1

    for i in range(num_t * clocks + delay):
        input_element = (input_element + 1) % clocks
        if input_element == 0:
            in_iterations += 1
        if i >= delay or output_element != -1:
            output_element = (output_element + 1) % clocks
            if output_element == 0:
                out_iterations += 1
        tester.print("clk: {}\n".format(clk))

        for k in range(len(graph.input_nodes[input_element].flat_idxs)):
            if not graph.input_nodes[input_element].flat_idxs[k].invalid:
                tester.poke(in_ports[k].port, graph.input_nodes[input_element].flat_idxs[k].idx + in_iterations)
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
                    out_ports[k].expect(graph.output_nodes[output_element].flat_idxs[k].idx + out_iterations)

#        tester.print("reshape write counter: %d\n", tester.circuit.reshape_write_counter)
#        tester.print("ram first valid write: %d\n \n", tester.circuit.first_valid)
        tester.step(2)
        clk += 1
    compile_and_run(tester)
