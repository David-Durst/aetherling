from aetherling.modules.permutation.assign_memory_addresses import *
from aetherling.modules.permutation.assign_banks import assign_banks
from aetherling.space_time.type_helpers import ST_Tombstone

def test_2_3_flip_mem_addresses():
    t_len = 3
    s_len = 2
    input_type = ST_SSeq(s_len, ST_TSeq(t_len, 0, ST_Tombstone()))
    output_type = ST_TSeq(t_len, 0, ST_SSeq(s_len, ST_Tombstone()))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_memory_addresses(assign_banks(graph))
    check_mem_addr(fixed_graph, s_len, t_len)

def test_4_6_flip_mem_addresses():
    t_len = 4
    s_len = 6
    input_type = ST_SSeq(s_len, ST_TSeq(t_len, 0, ST_Tombstone()))
    output_type = ST_TSeq(t_len, 0, ST_SSeq(s_len, ST_Tombstone()))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_memory_addresses(assign_banks(graph))
    check_mem_addr(fixed_graph, s_len, t_len)

def test_2_2_flip_mem_addresses():
    t_len = 2
    s_len = 2
    input_type = ST_SSeq(s_len, ST_TSeq(t_len, 0, ST_Tombstone()))
    output_type = ST_TSeq(t_len, 0, ST_SSeq(s_len, ST_Tombstone()))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_memory_addresses(assign_banks(graph))
    check_mem_addr(fixed_graph, s_len, t_len)

def test_10_10_flip_mem_addresses():
    t_len = 10
    s_len = 10
    input_type = ST_SSeq(s_len, ST_TSeq(t_len, 0, ST_Tombstone()))
    output_type = ST_TSeq(t_len, 0, ST_SSeq(s_len, ST_Tombstone()))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_memory_addresses(assign_banks(graph))
    check_mem_addr(fixed_graph, s_len, t_len)

def test_T2_S2_T2_banks():
    t_len_1 = 2
    t_len_2 = 2
    s_len = 2
    input_type = ST_TSeq(t_len_1, 0, ST_SSeq(s_len, ST_TSeq(t_len_2, 0, ST_Tombstone())))
    output_type = ST_TSeq(t_len_2, 0, ST_TSeq(t_len_1, 0, ST_SSeq(s_len, ST_Tombstone())))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_memory_addresses(assign_banks(graph))
    check_mem_addr(fixed_graph, s_len, t_len_1*t_len_2)

def test_T2_1_S2_T2_banks():
    t_len_1 = 2
    i_len_1 = 1
    t_len_2 = 2
    s_len = 2
    input_type = ST_TSeq(t_len_1, i_len_1, ST_SSeq(s_len, ST_TSeq(t_len_2, 0, ST_Tombstone())))
    output_type = ST_TSeq(t_len_2, 0, ST_TSeq(t_len_1, i_len_1, ST_SSeq(s_len, ST_Tombstone())))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_memory_addresses(assign_banks(graph))
    check_mem_addr(fixed_graph, s_len, (t_len_1+i_len_1)*t_len_2)

def test_different_port_widths_banks():
    input_type = ST_TSeq(2, 2, ST_SSeq(2, ST_Int()))
    output_type = ST_SSeq(4, ST_TSeq(1, 3, ST_Int()))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_memory_addresses(assign_banks(graph))
    check_mem_addr(fixed_graph, 4, 4)

def test_T4_S2_T4_banks():
    t_len_1 = 4
    t_len_2 = 4
    s_len = 2
    input_type = ST_TSeq(t_len_1, 0, ST_SSeq(s_len, ST_TSeq(t_len_2, 0, ST_Tombstone())))
    output_type = ST_TSeq(t_len_2, 0, ST_TSeq(t_len_1, 0, ST_SSeq(s_len, ST_Tombstone())))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_memory_addresses(assign_banks(graph))
    check_mem_addr(fixed_graph, s_len, t_len_1*t_len_2)

def test_T40_S6_T20_banks():
    t_len_1 = 40
    t_len_2 = 20
    s_len = 6
    input_type = ST_TSeq(t_len_1, 0, ST_SSeq(s_len, ST_TSeq(t_len_2, 0, ST_Tombstone())))
    output_type = ST_TSeq(t_len_2, 0, ST_TSeq(t_len_1, 0, ST_SSeq(s_len, ST_Tombstone())))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_memory_addresses(assign_banks(graph))
    check_mem_addr(fixed_graph, s_len, t_len_1*t_len_2)

def test_shared_sseq_2_tseq_3_1_diff_2_3_flip_banks():
    no = 2
    ni = 3
    ii = 0
    nii = 2
    input_type = ST_SSeq(no, ST_TSeq(3, 1, ST_TSeq(ni, ii, ST_SSeq(nii, ST_Int()))))
    output_type = ST_SSeq(no, ST_TSeq(3, 1, ST_SSeq(nii, ST_TSeq(ni, ii, ST_Int()))))
    graph = build_input_output_graph(input_type, output_type)
    fixed_graph = assign_memory_addresses(assign_banks(graph))
    check_mem_addr(fixed_graph, no*nii, (3+1)*(3+0))

def check_mem_addr(fixed_graph, s_len, t_len):
    free_addresses = [[True for _ in range(bank_size)] for bank_size in get_bank_sizes(fixed_graph)]
    first_output_latency = get_output_latencies(fixed_graph)[0]
    for t in range(t_len):
        # free any address written to at this point, which for t - first_output_latency for output
        # as output t is first_output_latency after current t
        output_t = t - first_output_latency
        if output_t >= 0:
            for s in range(s_len):
                if not fixed_graph.output_nodes[output_t].flat_idxs[s].invalid:
                    bank = fixed_graph.output_nodes[output_t].edge_banks[s]
                    addr = fixed_graph.output_nodes[output_t].edge_addr[s]
                    free_addresses[bank][addr] = True
        for s in range(s_len):
            # ensure that only writting to an address if it hasn't been written to yet or is currently being read from
            # ensure address that currently writing to matches the address that will be ready from
            # output_addr = get_output_address_at_input(t, s, fixed_graph.input_type, fixed_graph.output_type)
            bank = fixed_graph.input_nodes[t].edge_banks[s]
            addr = fixed_graph.input_nodes[t].edge_addr[s]
            output_addr = get_output_address_at_input(t, s, fixed_graph.input_type, fixed_graph.output_type)
            if not fixed_graph.input_nodes[t].flat_idxs[s].invalid:
                assert free_addresses[bank][addr]
                assert addr == fixed_graph.output_nodes[output_addr.t].edge_addr[output_addr.s]
                free_addresses[bank][addr] = False
