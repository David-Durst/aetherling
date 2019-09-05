from aetherling.modules.permutation.build_graph import *
from aetherling.modules.permutation.compute_latency import get_output_latencies, l_network
from dataclasses import dataclass
from typing import List
from heapq import heapify, heappop, heappush, nsmallest

@dataclass(order=True)
class Address:
    expire_time: int
    addr: int

def get_bank_size(graph: InputOutputGraph) -> int:
    """
    Given a bipartite graph, get the size of all banks needed to implement it
    :param graph: The graph to find the latency of
    :return: The bank size
    """
    output_latencies = get_output_latencies(graph)
    return output_latencies[0] - 2*l_network

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
    bank_size = get_bank_size(graph)
    num_banks = len(graph.input_nodes[0].edge_banks)
    for t in range(len(graph.input_nodes)):
        lifetimes += [[0] * num_banks]
        for s in range(num_banks):
            input_latency = t
            output_addr = get_output_address_at_input(t, s, graph.input_type, graph.output_type)
            output_latency = output_latencies[output_addr.t]
            lifetimes[t][s] = output_latency - input_latency - 2 * l_network

    # maintain a separate set of used and free addresses per bank
    used_addr_heaps = [[] for _ in range(num_banks)]
    free_addr_stacks = [[Address(-1, i) for i in range(bank_size)] for _ in range(num_banks)]
    for t in range(len(graph.input_nodes)):
        # adjustment from paper - need to get the input sequence writing to bank s on this clock,
        # not just input sequence s. Thus, get a list of the input sequences sorted by that
        cur_clock_input_node = graph.input_nodes[t]
        input_streams_and_banks_this_clock = [(cur_clock_input_node.edge_banks[s], s)
                                              for s in range(num_banks)]
        input_s_sorted_by_banks_this_clock = sorted(input_streams_and_banks_this_clock)
        for s in range(num_banks):
            if len(used_addr_heaps[s]) > 0:
                soonest_expiring_addr = nsmallest(1, used_addr_heaps[s])[0]
            else:
                soonest_expiring_addr = None
            while (soonest_expiring_addr is not None) and (soonest_expiring_addr.expire_time == t):
                soonest_expiring_addr = heappop(used_addr_heaps[s])
                free_addr_stacks[s].append(soonest_expiring_addr)
                soonest_expiring_addr = nsmallest(1, used_addr_heaps[s])[0]
            next_addr_to_write_to = free_addr_stacks[s].pop()
            # adjustment from paper, see above the adjustment
            (_, input_s) = input_s_sorted_by_banks_this_clock[s]
            graph.input_nodes[t].edge_addr[input_s] = next_addr_to_write_to.addr
            output_addr = get_output_address_at_input(t, input_s, graph.input_type, graph.output_type)
            graph.output_nodes[output_addr.t].edge_addr[output_addr.s] = next_addr_to_write_to.addr
            next_addr_to_write_to.expire_time = lifetimes[t][input_s] + t
            heappush(used_addr_heaps[s], next_addr_to_write_to)

    return graph



