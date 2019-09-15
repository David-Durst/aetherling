from .assign_banks import assign_banks
from .assign_memory_addresses import assign_memory_addresses
from .build_graph import build_input_output_graph, InputOutputGraph, BipartiteNode
from .compute_latency import get_output_latencies
from aetherling.space_time.space_time_types import ST_Type


def build_permutation_graph(t_in: ST_Type, t_out: ST_Type) -> InputOutputGraph:
    """
    Create the permutation graph with the banks and memory address assigned.
    :param t_in: the input type to the graph
    :param t_out: the output type to the graph
    :return: The graph with banks and memory address assigned.
    """
    unset_graph = build_input_output_graph(t_in, t_out)
    return assign_memory_addresses(assign_banks(unset_graph))

def get_latency(t_in: ST_Type, t_out: ST_Type) -> int:
    """
    Get the latency of a reshape between t_in and t_out.
    Latency is the clock cycle of the first output relative to the first input.
    :param t_in: the input type to the graph
    :param t_out: the output type to the graph
    :return: The latency of the reshape.
    """
    graph = build_permutation_graph(t_in, t_out)
    return get_output_latencies(graph)[0]
