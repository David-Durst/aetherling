from aetherling.modules.permutation.assign_memory_addresses import *
from aetherling.modules.permutation.assign_banks import assign_banks
from aetherling.space_time_modules.type_helpers import ST_Tombstone

def test_2_3_flip_mem_addresses():
    t_len = 3
    s_len = 2
    input_type = ST_SSeq(s_len, ST_TSeq(t_len, 0, ST_Tombstone()))
    output_type = ST_TSeq(t_len, 0, ST_SSeq(s_len, ST_Tombstone()))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_memory_addresses(assign_banks(graph))

    free_addresses = [[True] * get_bank_size(fixed_graph) for _ in range(s_len)]
    first_output_latency = get_output_latencies(fixed_graph)[0]
    for t in range(t_len):
        # free any address written to at this point, which for t - first_output_latency for output
        # as output t is first_output_latency after current t
        output_t = t - first_output_latency
        if output_t >= 0:
            for s in range(s_len):
                bank = fixed_graph.output_nodes[output_t].edge_banks[s]
                addr = fixed_graph.output_nodes[output_t].edge_addr[s]
                free_addresses[bank][addr] = True
        for s in range(s_len):
            # ensure that only writting to an address if it hasn't been written to yet or is currently being read from
            # ensure address that currently writing to matches the address that will be ready from
            # output_addr = get_output_address_at_input(t, s, fixed_graph.input_type, fixed_graph.output_type)
            bank = fixed_graph.input_nodes[t].edge_banks[s]
            addr = fixed_graph.input_nodes[t].edge_addr[s]
            output_addr = get_output_address_at_input(t, s, graph.input_type, graph.output_type)
            assert free_addresses[bank][addr]
            assert addr == fixed_graph.output_nodes[output_addr.t].edge_addr[output_addr.s]
            free_addresses[bank][addr] = False
