from aetherling.modules.permutation.compute_latency import *
from aetherling.space_time_modules.type_helpers import ST_Tombstone

def test_flip_latency():
    input_type = ST_SSeq(2, ST_TSeq(3, 0,  ST_Tombstone()))
    output_type = ST_TSeq(3, 0, ST_SSeq(2, ST_Tombstone()))
    graph = build_input_output_graph(input_type, output_type)
    output_latencies = get_output_latencies(graph)
    assert output_latencies == [2,3,4]

def test_nested_vs_unnested_invalids_latency():
    input_type = ST_TSeq(2, 0, ST_TSeq(1, 1,  ST_Tombstone()))
    output_type = ST_TSeq(2, 2, ST_Tombstone())
    graph = build_input_output_graph(input_type, output_type)
    output_latencies = get_output_latencies(graph)
    assert output_latencies == [2,3,4,5]

def test_invalids_not_affecting_latency():
    input_type = ST_TSeq(2, 2, ST_Tombstone())
    output_type = ST_TSeq(2, 0, ST_TSeq(1, 1,  ST_Tombstone()))
    graph = build_input_output_graph(input_type, output_type)
    output_latencies = get_output_latencies(graph)
    assert output_latencies == [1,2,3,4]

def test_different_port_widths_latency():
    input_type = ST_TSeq(2, 2, ST_SSeq(2, ST_Int()))
    output_type = ST_SSeq(4, ST_TSeq(1, 3, ST_Int()))
    graph = build_input_output_graph(input_type, output_type)
    output_latencies = get_output_latencies(graph)
    assert output_latencies == [2,3,4,5]

def test_different_port_widths_not_affecting_latency():
    input_type = ST_TSeq(1, 1, ST_SSeq(2, ST_Int()))
    output_type = ST_SSeq(1, ST_TSeq(2, 0, ST_Int()))
    graph = build_input_output_graph(input_type, output_type)
    output_latencies = get_output_latencies(graph)
    assert output_latencies == [1,2]

def test_T4_S2_T4_banks():
    t_len_1 = 4
    t_len_2 = 4
    s_len = 2
    input_type = ST_TSeq(t_len_1, 0, ST_SSeq(s_len, ST_TSeq(t_len_2, 0, ST_Tombstone())))
    output_type = ST_TSeq(t_len_2, 0, ST_TSeq(t_len_1, 0, ST_SSeq(s_len, ST_Tombstone())))
    graph = build_input_output_graph(input_type, output_type)
    output_latencies = get_output_latencies(graph)
    assert output_latencies == [3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18]
