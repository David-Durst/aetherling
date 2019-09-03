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
    return max(list(map(lambda x: x.t, o.neighbors))) + l_memory + 2*l_network

def get_output_latencies(graph: InputOutputGraph) -> List[int]:
    """
    Return the clock cycles (latencies relative to clock 0) of each output node in the bipartite graph
    :param graph: the bipartite graph of per clock cycle inputs and outputs
    :return: the latency of each output node in the graph
    """
    # latencies without accounting for misorderd outputs or holes
    per_node_min_latencies = list(map(get_latency_for_output, graph.output_nodes))
    # make sure that each output comes after its predecessors
    latencies_with_causality = [per_node_min_latencies[0]] + \
        [per_node_min_latencies[i-1] + 1 if per_node_min_latencies[i] <= per_node_min_latencies[i-1] else per_node_min_latencies[i]
         for i in range(1,len(per_node_min_latencies))]
    # remove all holes, reverse with [::-1] at end to get latencies in right order
    return ([latencies_with_causality[-1]] +
            [latencies_with_causality[i+1] - 1 for i in range(len(latencies_with_causality) - 2, -1, -1)])[::-1]
