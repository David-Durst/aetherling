from aetherling.modules.permutation.assign_ts_addresses import *
from aetherling.modules.permutation.build_graph import *
from itertools import accumulate, groupby
from functools import reduce
from dataclasses import dataclass
from typing import List

# latency between a read and write, l_M in the paper
l_memory = 1
# latency of sorting network, l_N in the paper
l_network = 0

def get_latency_for_output(o: BipartiteNode) -> int:
    """
    Given an output node of a bipartite graph, get its minimum latency in order for it to come
    after all the inputs in its neighborhood
    :param o:
    :return: the latency, the first clock cycle where the output can be emitted without violating
    causality
    """
    # don't count nodes that are invalid, don't need to wait for them to arrive
    return max(list(map(lambda x: x.t, [node for node in o.neighbors if not node.flat_idx.invalid])), default=0) + l_memory + 2*l_network

def get_output_latencies(graph: InputOutputGraph) -> List[int]:
    """
    Return the clock cycles (latencies relative to clock 0) of each output node in the bipartite graph
    :param graph: the bipartite graph of per clock cycle inputs and outputs
    :return: the latency of each output node in the graph
    """
    # latencies without accounting for misorderd outputs or holes
    per_node_min_latencies = list(map(get_latency_for_output, graph.output_nodes))
    # make sure that each output comes after its predecessors
    latencies_with_causality = [per_node_min_latencies[0]]
    for i in range(1, len(per_node_min_latencies)):
        if per_node_min_latencies[i] <= latencies_with_causality[-1]:
            latencies_with_causality += [latencies_with_causality[-1] + 1]
        else:
            latencies_with_causality += [per_node_min_latencies[i]]
    # remove all holes, last element in a fixed position, all others contiguous leading to it
    last_element_latency = latencies_with_causality[-1]
    first_element_latency = last_element_latency - (len(latencies_with_causality) - 1)
    return list(range(first_element_latency, last_element_latency + 1))
