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
    for i in range(num_t):
        output_counter = 0
        for j in range(clocks + delay):
            tester.print("clk: {}\n".format(clk))
            # tester.print("last: %d\n", tester.circuit.last)
            if j < clocks:
                for k in range(len(graph.input_nodes[j].flat_idxs)):
                    tester.circuit.I[k] = graph.input_nodes[j].flat_idxs[k].idx
            tester.eval()
            if j < clocks:
                for k in range(len(graph.input_nodes[j].flat_idxs)):
                    tester.print("ram_wr {}: %d\n".format(k), tester.circuit.ram_wr[k])
                    tester.print("addr_wr {}: %d\n".format(k), tester.circuit.addr_wr[k])
                for k in range(len(graph.output_nodes[output_counter].flat_idxs)):
                    #tester.circuit.O[k].expect(graph.input_nodes[output_counter].flat_idxs[k].idx)
                    tester.print("output {}: %d\n".format(k), tester.circuit.O[k])
                    tester.print("ram_rd {}: %d\n".format(k), tester.circuit.ram_rd[k])
                    tester.print("addr_rd {}: %d\n".format(k), tester.circuit.addr_rd[k])
            if j >= delay:
                output_counter += 1
            tester.print("ram first valid write: %d\n", tester.circuit.first_valid)
            tester.step(2)
            if j < clocks:
                for k in range(len(graph.input_nodes[j].flat_idxs)):
                    continue
                    #tester.print("ram_wr {}: %d\n".format(k), tester.circuit.ram_wr[k])
                    #tester.print("addr_wr {}: %d\n".format(k), tester.circuit.addr_wr[k])
            clk += 1
    tester.circuit.O[0].expect(1)
    compile_and_run(tester)
