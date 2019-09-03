from aetherling.modules.permutation.compute_latency import *
from aetherling.space_time_modules.type_helpers import ST_Tombstone

def test_flip_latency():
    input_type = ST_SSeq(2, ST_TSeq(3, 0,  ST_Tombstone()))
    output_type = ST_TSeq(3, 0, ST_SSeq(2, ST_Tombstone()))
    graph = build_input_output_graph(input_type, output_type)
    output_latencies = get_output_latencies(graph)
    assert output_latencies == [2,3,4]

