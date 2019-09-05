from aetherling.modules.permutation.build_graph import *
from aetherling.space_time_modules.type_helpers import ST_Tombstone


def test_flip_graph():
    input_type = ST_SSeq(2, ST_TSeq(3, 0, ST_Tombstone()))
    output_type = ST_TSeq(3, 0, ST_SSeq(2, ST_Tombstone()))
    graph = build_input_output_graph(input_type, output_type)
    correct_result = InputOutputGraph(
        input_nodes=[
            BipartiteNode(idx=0, flat_idxs=[FlatIndex(invalid=False, idx=0), FlatIndex(invalid=False, idx=3)],
                          neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=0), s=0, t=0),
                                     SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=3), s=1, t=1)],
                          edge_banks=[0, 1],
                          edge_addr=[-1, -1]),
            BipartiteNode(idx=1, flat_idxs=[FlatIndex(invalid=False, idx=1), FlatIndex(invalid=False, idx=4)],
                          neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=1), s=1, t=0),
                                     SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=4), s=0, t=2)],
                          edge_banks=[0, 1],
                          edge_addr=[-1, -1]),
            BipartiteNode(idx=2, flat_idxs=[FlatIndex(invalid=False, idx=2), FlatIndex(invalid=False, idx=5)],
                          neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=2), s=0, t=1),
                                     SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=5), s=1, t=2)],
                          edge_banks=[0, 1],
                          edge_addr=[-1, -1])],
        input_type=ST_SSeq(n=2, t=ST_TSeq(n=3, i=0, t=ST_Tombstone())),
        output_nodes=[
            BipartiteNode(idx=0, flat_idxs=[FlatIndex(invalid=False, idx=0), FlatIndex(invalid=False, idx=1)],
                          neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=0), s=0, t=0),
                                     SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=1), s=0, t=1)],
                          edge_banks=[0, 0], edge_addr=[-1, -1]),
            BipartiteNode(idx=1, flat_idxs=[FlatIndex(invalid=False, idx=2), FlatIndex(invalid=False, idx=3)],
                          neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=2), s=0, t=2),
                                     SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=3), s=1, t=0)],
                          edge_banks=[0, 1], edge_addr=[-1, -1]),
            BipartiteNode(idx=2, flat_idxs=[FlatIndex(invalid=False, idx=4), FlatIndex(invalid=False, idx=5)],
                          neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=4), s=1, t=1),
                                     SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=5), s=1, t=2)],
                          edge_banks=[1, 1], edge_addr=[-1, -1])],
        output_type=ST_TSeq(n=3, i=0, t=ST_SSeq(n=2, t=ST_Tombstone())))
    assert graph == correct_result


def test_flip_graph_invalids():
    input_type = ST_SSeq(2, ST_TSeq(3, 1, ST_Tombstone()))
    output_type = ST_TSeq(3, 1, ST_SSeq(2, ST_Tombstone()))
    graph = build_input_output_graph(input_type, output_type)
    correct_result = InputOutputGraph(
        input_nodes=[
            BipartiteNode(idx=0, flat_idxs=[FlatIndex(invalid=False, idx=0), FlatIndex(invalid=False, idx=3)],
                          neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=0), s=0, t=0),
                                     SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=3), s=1, t=1)],
                          edge_banks=[0, 1], edge_addr=[-1, -1]),
            BipartiteNode(idx=1, flat_idxs=[FlatIndex(invalid=False, idx=1), FlatIndex(invalid=False, idx=4)],
                          neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=1), s=1, t=0),
                                     SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=4), s=0, t=2)],
                          edge_banks=[0, 1], edge_addr=[-1, -1]),
            BipartiteNode(idx=2, flat_idxs=[FlatIndex(invalid=False, idx=2), FlatIndex(invalid=False, idx=5)],
                          neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=2), s=0, t=1),
                                     SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=5), s=1, t=2)],
                          edge_banks=[0, 1], edge_addr=[-1, -1]),
            BipartiteNode(idx=3, flat_idxs=[FlatIndex(invalid=True, idx=0), FlatIndex(invalid=True, idx=1)],
                          neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=True, idx=0), s=0, t=3),
                                     SpaceTimeIndex(flat_idx=FlatIndex(invalid=True, idx=1), s=1, t=3)],
                          edge_banks=[0, 1], edge_addr=[-1, -1])],
        input_type=ST_SSeq(n=2, t=ST_TSeq(n=3, i=1, t=ST_Tombstone())),
        output_nodes=[
            BipartiteNode(idx=0, flat_idxs=[FlatIndex(invalid=False, idx=0), FlatIndex(invalid=False, idx=1)],
                          neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=0), s=0, t=0),
                                     SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=1), s=0, t=1)],
                          edge_banks=[0, 0], edge_addr=[-1, -1]),
            BipartiteNode(idx=1, flat_idxs=[FlatIndex(invalid=False, idx=2), FlatIndex(invalid=False, idx=3)],
                          neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=2), s=0, t=2),
                                     SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=3), s=1, t=0)],
                          edge_banks=[0, 1], edge_addr=[-1, -1]),
            BipartiteNode(idx=2, flat_idxs=[FlatIndex(invalid=False, idx=4), FlatIndex(invalid=False, idx=5)],
                          neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=4), s=1, t=1),
                                     SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=5), s=1, t=2)],
                          edge_banks=[1, 1], edge_addr=[-1, -1]),
            BipartiteNode(idx=3, flat_idxs=[FlatIndex(invalid=True, idx=0), FlatIndex(invalid=True, idx=1)],
                          neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=True, idx=0), s=0, t=3),
                                     SpaceTimeIndex(flat_idx=FlatIndex(invalid=True, idx=1), s=1, t=3)],
                          edge_banks=[0, 1], edge_addr=[-1, -1])],
        output_type=ST_TSeq(n=3, i=1, t=ST_SSeq(n=2, t=ST_Tombstone())))
    assert graph == correct_result

def test_differnt_port_widths_graph():
    input_type = ST_TSeq(1, 1, ST_SSeq(2, ST_Int()))
    output_type = ST_SSeq(1, ST_TSeq(2, 0, ST_Int()))
    graph = build_input_output_graph(input_type, output_type)
    correct_result = InputOutputGraph(
        input_nodes=[BipartiteNode(idx=0, flat_idxs=[FlatIndex(invalid=False, idx=0), FlatIndex(invalid=False, idx=1)],
                                   neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=0), s=0, t=0),
                                              SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=1), s=0, t=1)],
                                   edge_banks=[0, 1], edge_addr=[-1, -1]),
                     BipartiteNode(idx=1, flat_idxs=[FlatIndex(invalid=True, idx=0), FlatIndex(invalid=True, idx=1)],
                                   neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=True, idx=0), s=1, t=0),
                                              SpaceTimeIndex(flat_idx=FlatIndex(invalid=True, idx=1), s=1, t=1)],
                                   edge_banks=[0, 1], edge_addr=[-1, -1])],
        input_type=ST_TSeq(n=1, i=1, t=ST_SSeq(n=2, t=ST_Int())),
        output_nodes=[BipartiteNode(idx=0, flat_idxs=[FlatIndex(invalid=False, idx=0), FlatIndex(invalid=True, idx=0)],
                                    neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=0), s=0, t=0),
                                               SpaceTimeIndex(flat_idx=FlatIndex(invalid=True, idx=0), s=0, t=1)],
                                    edge_banks=[0, 0], edge_addr=[-1, -1]),
                      BipartiteNode(idx=1, flat_idxs=[FlatIndex(invalid=False, idx=1), FlatIndex(invalid=True, idx=1)],
                                    neighbors=[SpaceTimeIndex(flat_idx=FlatIndex(invalid=False, idx=1), s=1, t=0),
                                               SpaceTimeIndex(flat_idx=FlatIndex(invalid=True, idx=1), s=1, t=1)],
                                    edge_banks=[1, 1], edge_addr=[-1, -1])],
        output_type=ST_SSeq(n=1, t=ST_TSeq(n=2, i=0, t=ST_Int())))
    assert graph == correct_result

