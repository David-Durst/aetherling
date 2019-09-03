from aetherling.modules.permutation.build_graph import *
from aetherling.space_time_modules.type_helpers import ST_Tombstone

def test_flip_graph():
    input_type = ST_SSeq(2, ST_TSeq(3, 0,  ST_Tombstone()))
    output_type = ST_TSeq(3, 0, ST_SSeq(2, ST_Tombstone()))
    graph = build_input_output_graph(input_type, output_type)
    correct_result = InputOutputGraph(
        input_nodes=[
            BipartiteNode(idx=0, flat_idxs=[0, 3],
                          neighbors=[SpaceTimeIndex(flat_idx=0, s=0, t=0), SpaceTimeIndex(flat_idx=3, s=1, t=1)],
                          edge_colors=[0, 1]),
            BipartiteNode(idx=1, flat_idxs=[1, 4],
                          neighbors=[SpaceTimeIndex(flat_idx=1, s=1, t=0), SpaceTimeIndex(flat_idx=4, s=0, t=2)],
                          edge_colors=[0, 1]),
            BipartiteNode(idx=2, flat_idxs=[2, 5],
                          neighbors=[SpaceTimeIndex(flat_idx=2, s=0, t=1), SpaceTimeIndex(flat_idx=5, s=1, t=2)],
                          edge_colors=[0, 1])],
        output_nodes=[
            BipartiteNode(idx=0, flat_idxs=[0, 1],
                          neighbors=[SpaceTimeIndex(flat_idx=0, s=0, t=0), SpaceTimeIndex(flat_idx=1, s=0, t=1)],
                          edge_colors=[0, 1]),
            BipartiteNode(idx=1, flat_idxs=[2, 3],
                          neighbors=[SpaceTimeIndex(flat_idx=2, s=0, t=2), SpaceTimeIndex(flat_idx=3, s=1, t=0)],
                          edge_colors=[0, 1]),
            BipartiteNode(idx=2, flat_idxs=[4, 5],
                          neighbors=[SpaceTimeIndex(flat_idx=4, s=1, t=1), SpaceTimeIndex(flat_idx=5, s=1, t=2)],
                          edge_colors=[0, 1])])
    assert graph == correct_result
