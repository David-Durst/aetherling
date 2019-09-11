from aetherling.modules.permutation.assign_banks import *
from aetherling.modules.permutation import build_permutation_graph
from aetherling.space_time.type_helpers import ST_Tombstone
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
    check_reshape(graph, 2, testcircuit.output_delay, tester)

def check_reshape(graph: InputOutputGraph, num_t, delay, tester, has_ce = False, has_reset = False):
    clocks = len(graph.input_nodes)
    if has_ce:
        tester.circuit.CE = True
    if has_reset:
        tester.circuit.reset = False
    clk = 0
    output_clock = 0
    for i in range(num_t * clocks + delay):
        input_clock = i % clocks
        tester.print("clk: {}\n".format(clk))

        for k in range(len(graph.input_nodes[input_clock].flat_idxs)):
            tester.circuit.I[k] = graph.input_nodes[input_clock].flat_idxs[k].idx

        tester.eval()

        for k in range(len(graph.input_nodes[input_clock].flat_idxs)):
            tester.print("ram_wr {}: %d\n".format(k), tester.circuit.ram_wr[k])
            tester.print("addr_wr {}: %d\n".format(k), tester.circuit.addr_wr[k])

        for k in range(len(graph.output_nodes[output_clock].flat_idxs)):
            #tester.circuit.O[k].expect(graph.output_nodes[output_clock].flat_idxs[k].idx)
            tester.print("output {}: %d\n".format(k), tester.circuit.O[k])
            tester.print("ram_rd {}: %d\n".format(k), tester.circuit.ram_rd[k])
            tester.print("addr_rd {}: %d\n".format(k), tester.circuit.addr_rd[k])

        if i >= delay:
            for k in range(len(graph.output_nodes[output_clock].flat_idxs)):
                tester.circuit.O[k].expect(graph.output_nodes[output_clock].flat_idxs[k].idx)
            output_clock = (output_clock + 1) % clocks

        tester.print("ram first valid write: %d\n", tester.circuit.first_valid)
        tester.step(2)
        clk += 1
    compile_and_run(tester)
