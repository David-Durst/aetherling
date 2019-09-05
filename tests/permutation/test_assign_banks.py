from aetherling.modules.permutation.assign_banks import *
from aetherling.space_time.type_helpers import ST_Tombstone


def test_2_3_flip_banks():
    t_len = 3
    s_len = 2
    input_type = ST_SSeq(s_len, ST_TSeq(t_len, 0, ST_Tombstone()))
    output_type = ST_TSeq(t_len, 0, ST_SSeq(s_len, ST_Tombstone()))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_banks(graph)
    check_banks(fixed_graph, s_len, t_len)


def test_4_6_flip_banks():
    t_len = 4
    s_len = 6
    input_type = ST_SSeq(s_len, ST_TSeq(t_len, 0, ST_Tombstone()))
    output_type = ST_TSeq(t_len, 0, ST_SSeq(s_len, ST_Tombstone()))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_banks(graph)
    check_banks(fixed_graph, s_len, t_len)

def test_T4_S2_T4_banks():
    t_len_1 = 4
    t_len_2 = 4
    s_len = 2
    input_type = ST_TSeq(t_len_1, 0, ST_SSeq(s_len, ST_TSeq(t_len_2, 0, ST_Tombstone())))
    output_type = ST_TSeq(t_len_2, 0, ST_TSeq(t_len_1, 0, ST_SSeq(s_len, ST_Tombstone())))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_banks(graph)
    check_banks(fixed_graph, s_len, t_len_1*t_len_2)

def test_T4_2_S2_T4_banks():
    t_len_1 = 4
    i_len_1 = 2
    t_len_2 = 4
    s_len = 2
    input_type = ST_TSeq(t_len_1, i_len_1, ST_SSeq(s_len, ST_TSeq(t_len_2, 0, ST_Tombstone())))
    output_type = ST_TSeq(t_len_2, 0, ST_TSeq(t_len_1, i_len_1, ST_SSeq(s_len, ST_Tombstone())))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_banks(graph)
    check_banks(fixed_graph, s_len, (t_len_1+i_len_1)*t_len_2)

def test_T2_1_S2_T2_banks():
    t_len_1 = 2
    i_len_1 = 1
    t_len_2 = 2
    s_len = 2
    input_type = ST_TSeq(t_len_1, i_len_1, ST_SSeq(s_len, ST_TSeq(t_len_2, 0, ST_Tombstone())))
    output_type = ST_TSeq(t_len_2, 0, ST_TSeq(t_len_1, i_len_1, ST_SSeq(s_len, ST_Tombstone())))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_banks(graph)
    check_banks(fixed_graph, s_len, (t_len_1+i_len_1)*t_len_2)

def test_different_port_widths_banks():
    input_type = ST_TSeq(2, 2, ST_SSeq(2, ST_Int()))
    output_type = ST_SSeq(4, ST_TSeq(1, 3, ST_Int()))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_banks(graph)
    check_banks(fixed_graph, 4, 4)

def test_T40_S6_T20_banks():
    t_len_1 = 40
    t_len_2 = 20
    s_len = 6
    input_type = ST_TSeq(t_len_1, 0, ST_SSeq(s_len, ST_TSeq(t_len_2, 0, ST_Tombstone())))
    output_type = ST_TSeq(t_len_2, 0, ST_TSeq(t_len_1, 0, ST_SSeq(s_len, ST_Tombstone())))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_banks(graph)
    check_banks(fixed_graph, s_len, t_len_1*t_len_2)


def check_banks(fixed_graph, s_len, t_len):
    for t in range(t_len):
        # assert that all banks are used and are unique so no conflicts on input or output
        assert sorted(fixed_graph.input_nodes[t].edge_banks) == list(range(s_len))
        assert sorted(fixed_graph.output_nodes[t].edge_banks) == list(range(s_len))
        for s in range(s_len):
            output_addr = get_output_address_at_input(t, s, fixed_graph.input_type, fixed_graph.output_type)
            assert fixed_graph.input_nodes[t].edge_banks[s] == \
                   fixed_graph.output_nodes[output_addr.t].edge_banks[output_addr.s]

