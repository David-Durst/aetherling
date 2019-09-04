from aetherling.modules.permutation.assign_banks import *
from aetherling.space_time_modules.type_helpers import ST_Tombstone


def test_2_3_flip_banks():
    t_len = 3
    s_len = 2
    input_type = ST_SSeq(s_len, ST_TSeq(t_len, 0, ST_Tombstone()))
    output_type = ST_TSeq(t_len, 0, ST_SSeq(s_len, ST_Tombstone()))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_banks(graph)
    for t in range(t_len):
        # assert that all banks are used and are unique so no conflicts on input or output
        assert sorted(fixed_graph.input_nodes[t].edge_banks) == list(range(s_len))
        assert sorted(fixed_graph.output_nodes[t].edge_banks) == list(range(s_len))
        for s in range(s_len):
            output_addr = get_output_address_at_input(t, s, fixed_graph.input_type, fixed_graph.output_type)
            assert fixed_graph.input_nodes[t].edge_banks[s] == \
                   fixed_graph.output_nodes[output_addr.t].edge_banks[output_addr.s]

def test_4_6_flip_banks():
    t_len = 4
    s_len = 6
    input_type = ST_SSeq(s_len, ST_TSeq(t_len, 0, ST_Tombstone()))
    output_type = ST_TSeq(t_len, 0, ST_SSeq(s_len, ST_Tombstone()))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_banks(graph)
    for t in range(t_len):
        # assert that all banks are used and are unique so no conflicts on input or output
        assert sorted(fixed_graph.input_nodes[t].edge_banks) == list(range(s_len))
        assert sorted(fixed_graph.output_nodes[t].edge_banks) == list(range(s_len))
        for s in range(s_len):
            output_addr = get_output_address_at_input(t, s, fixed_graph.input_type, fixed_graph.output_type)
            assert fixed_graph.input_nodes[t].edge_banks[s] == \
                   fixed_graph.output_nodes[output_addr.t].edge_banks[output_addr.s]

def test_T40_S6_T20_banks():
    t_len_1 = 40
    t_len_2 = 20
    s_len = 6
    input_type = ST_TSeq(t_len_1, 0, ST_SSeq(s_len, ST_TSeq(t_len_2, 0, ST_Tombstone())))
    output_type = ST_TSeq(t_len_2, 0, ST_TSeq(t_len_1, 0, ST_SSeq(s_len, ST_Tombstone())))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_banks(graph)
    for t in range(t_len_1 * t_len_2):
        # assert that all banks are used and are unique so no conflicts on input or output
        assert sorted(fixed_graph.input_nodes[t].edge_banks) == list(range(s_len))
        assert sorted(fixed_graph.output_nodes[t].edge_banks) == list(range(s_len))
        for s in range(s_len):
            output_addr = get_output_address_at_input(t, s, fixed_graph.input_type, fixed_graph.output_type)
            assert fixed_graph.input_nodes[t].edge_banks[s] == \
                   fixed_graph.output_nodes[output_addr.t].edge_banks[output_addr.s]
