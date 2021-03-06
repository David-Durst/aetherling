from aetherling.modules.permutation.build_graph import *
from aetherling.modules.permutation.compute_latency import get_output_latencies, l_network
from dataclasses import dataclass, replace
from typing import List
from heapq import heapify, heappop, heappush, nsmallest

@dataclass(order=True)
class Address:
    expire_time: int
    addr: int

class FreeAddressStack:
    next_addr: Address
    available_addr: List[Address]

    def __init__(self):
        self.next_addr = Address(-1, 0)
        self.available_addr = []

    def get_next_addr(self) -> Address:
        if len(self.available_addr) == 0:
            addr_to_return = self.next_addr
            self.next_addr = replace(self.next_addr, addr=addr_to_return.addr + 1)
            return addr_to_return
        else:
            return self.available_addr.pop()

    def add_free_addr(self, free_addr: Address):
        self.available_addr.append(free_addr)


def get_bank_sizes(graph: InputOutputGraph) -> List[int]:
    """
    Given a bipartite graph, get the size of each bank needed to implement it
    :param graph: The graph to find the bank sizes of
    :return: The bank sizes
    """
    num_banks = len(graph.input_nodes[0].edge_banks)
    max_bank_addr = [0 for _ in range(num_banks)]
    for t in range(len(graph.input_nodes)):
        for s in range(num_banks):
            cur_bank = graph.input_nodes[t].edge_banks[s]
            cur_addr = graph.input_nodes[t].edge_addr[s]
            max_bank_addr[cur_bank] = max(max_bank_addr[cur_bank], cur_addr)
    return list(map(lambda x : x + 1, max_bank_addr))

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
    num_banks = len(graph.input_nodes[0].edge_banks)
    for t in range(len(graph.input_nodes)):
        lifetimes += [[0] * num_banks]
        for s in range(num_banks):
            input_latency = t
            output_addr = get_output_address_at_input(t, s, graph.input_type, graph.output_type)
            output_latency = output_latencies[output_addr.t]
            if not output_addr.flat_idx.invalid:
                lifetimes[t][s] = output_latency - input_latency - 2 * l_network

    # maintain a separate set of used and free addresses per bank
    used_addr_heaps = [[] for _ in range(num_banks)]
    free_addr_stacks = [FreeAddressStack() for _ in range(num_banks)]
    for t in range(len(graph.input_nodes)):
        # adjustment from paper - need to get the input sequence writing to bank s on this clock,
        # not just input sequence s. Thus, get a list of the input sequences sorted by that
        cur_clock_input_node = graph.input_nodes[t]
        input_streams_and_banks_this_clock = [(cur_clock_input_node.edge_banks[s], s)
                                              for s in range(num_banks)]
        input_s_sorted_by_banks_this_clock = sorted(input_streams_and_banks_this_clock)
        for s in range(num_banks):
            # free addresses that are being read this clock
            if len(used_addr_heaps[s]) > 0:
                soonest_expiring_addr = nsmallest(1, used_addr_heaps[s])[0]
            else:
                soonest_expiring_addr = None
            while (soonest_expiring_addr is not None) and (soonest_expiring_addr.expire_time == t):
                soonest_expiring_addr = heappop(used_addr_heaps[s])
                free_addr_stacks[s].add_free_addr(soonest_expiring_addr)
                if len(used_addr_heaps[s]) > 0:
                    soonest_expiring_addr = nsmallest(1, used_addr_heaps[s])[0]
                else:
                    soonest_expiring_addr = None

            # adjustment from paper, see above the adjustment - why is this giving me the wrong s? wrong liftetime comes from this, meaning addr can be reused too early
            (_, input_s) = input_s_sorted_by_banks_this_clock[s]
            # get an address to write to
            # note: checking if the input is valid using input_s
            # and if valid, then getting the addr for bank s, not for input_s
            if graph.input_nodes[t].flat_idxs[input_s].invalid:
                next_addr_to_write_to = Address(0, 0)
            else:
                next_addr_to_write_to = free_addr_stacks[s].get_next_addr()
            graph.input_nodes[t].edge_addr[input_s] = next_addr_to_write_to.addr
            output_addr = get_output_address_at_input(t, input_s, graph.input_type, graph.output_type)
            graph.output_nodes[output_addr.t].edge_addr[output_addr.s] = next_addr_to_write_to.addr
            if not graph.input_nodes[t].flat_idxs[s].invalid:
                next_addr_to_write_to.expire_time = lifetimes[t][input_s] + t
                heappush(used_addr_heaps[s], next_addr_to_write_to)

    return graph



