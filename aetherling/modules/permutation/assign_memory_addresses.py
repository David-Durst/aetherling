from aetherling.modules.permutation.build_graph import *
from aetherling.modules.permutation.compute_latency import get_output_latencies
from dataclasses import dataclass
from typing import List

def assign_memory_addresses(graph: InputOutputGraph) -> InputOutputGraph:
    """
    Given a bipartite graph, fix the address¢ assignments so that:
    1. The same value is written to and read from the same address
    2. There are no address conflicts - each address on each bank is only being used by at most one value every clock

    :param graph: The graph to fix
    :return: a fixed graph
    """
    # first compute lifetimes of each value - how long they are stored in memory
    lifetimes = []
    output_latencies = get_output_latencies(graph)
    for t in len(graph.input_nodes):
        lifetimes += [0] * len(graph.input_nodes[0])
        for s in len(graph.input_nodes[0]):
            input_latency = t
            output_addr = get_output_address_at_input(t, s, graph.input_type, graph.output_type)
            output_latency = output_latencies[output_addr.t]
            lifetimes =


