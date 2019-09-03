from aetherling.modules.permutation.assign_addresses import *
from dataclasses import dataclass
from typing import List

@dataclass
class BipartiteNode:
    """
    A node in the bipartite graph representing input or outputs.
    Each node is one clock cycle's input or output
    """
    idx: int
    # these arrays have corresponding values at same indices
    # must be same length
    flat_idxs: List[int]
    # a neighbor can appear multiple times, once per shared value
    neighbors: List[SpaceTimeIndex]
    edge_colors: List[int]

@dataclass
class InputOutputGraph:
    """
    A graph of input and output nodes representing the per clock operations of a permutation
    """
    input_nodes: List[BipartiteNode]
    output_nodes: List[BipartiteNode]

def build_input_output_graph(input_type, output_type):
    """
    Build a bipartite graph of BipartiteNodes where each node is an input or output clock
    and an edge is a shared memory address between a read and a write for the input and output
    :param input_type: input type for permutation
    :param output_type: output type for permutation
    :return: An InputOutputGraph
    """
    if input_type.time() != output_type.time():
        raise Exception("input_type " ++ str(input_type) ++ " output_type " ++ str(output_type) ++ " times don't match.")
    if input_type.length() != output_type.length():
        raise Exception("input_type " ++ str(input_type) ++ " output_type " ++ str(output_type) ++ " lengths don't match.")
    if input_type.port_width() != output_type.port_width():
        raise Exception("input_type " ++ str(input_type) ++ " output_type " ++ str(output_type) ++ " port_widths don't match.")
    num_nodes_per_side = input_type.time()
    elements_per_clock = input_type.port_width()
    graph = InputOutputGraph([BipartiteNode(t, [], [], []) for t in range(num_nodes_per_side)],
                             [BipartiteNode(t, [], [], []) for t in range(num_nodes_per_side)])
    # for each t and s, add the edges
    for t in range(num_nodes_per_side):
        for s in range(elements_per_clock):
            # add data to input part of graph
            output_addr = get_output_address_at_input(t, s, input_type, output_type)
            graph.input_nodes[t].flat_idxs += [output_addr.flat_idx]
            graph.input_nodes[t].neighbors += [output_addr]
            graph.input_nodes[t].edge_colors += [s]
            # add data to output part of graph
            input_addr = get_input_address_at_output(t, s, input_type, output_type)
            graph.output_nodes[t].flat_idxs += [input_addr.flat_idx]
            graph.output_nodes[t].neighbors += [input_addr]
            graph.output_nodes[t].edge_colors += [s]

    return graph
