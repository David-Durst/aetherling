from aetherling.modules.permutation.build_graph import *
from dataclasses import dataclass
from typing import List

@dataclass
class BankConflict():
    """
    Whether there is a conflict.
    If there is a conflict, the coordinates of one of the conflicting values and it's new bank>
    """
    is_conflict: bool
    t: int
    s: int
    new_bank: int

def check_conflict(t: int, edge_banks: List[int]) -> BankConflict:
    """
    Given a list of banks used in one clock, check if there is a conflict
    :param t: The current clock
    :param edge_banks: The banks used in the current clock
    :return: Whether a conflict exists and one of the elements to change to fix the conflict
    """
    used_banks_this_clock = []
    for s in range(edge_banks):
        if edge_banks[s] in used_banks_this_clock:
            return BankConflict(True, t, s, (s + 1) % len(edge_banks))
        else:
            used_banks_this_clock += edge_banks[s]
    return BankConflict(False, -1, -1, -1)

def find_conflict(table: List[BipartiteNode]) -> BankConflict:
    """
    Given an a table of the used colors/banks per clock, find a conflict where a bank is used twice
    :param table: Either the output_nodes or input_nodes of an InputOutputGraph
    :return: Whether a conflict exists and one of the elements to change to fix the conflict
    """
    for t in range(len(table)):
        conflict = check_conflict(t, table[t].edge_banks)
        if conflict.is_conflict:
            return conflict
#        used_banks_this_clock = []
#        for s in range(len(table[t].edge_banks)):
#            if table[t].edge_banks[s] in used_banks_this_clock:
#                return BankConflict(True, t, s, (s + 1) % len(table[t].edge_banks))
#            else:
#                used_banks_this_clock += table[t].edge_banks[s]
    return BankConflict(False, -1, -1, -1)

def resolve_conflict(graph: InputOutputGraph, conflict: BankConflict, is_input: bool) -> InputOutputGraph:
    """
    Given a conflict:
    1. Change the bank of one of the two conflicting values in the input or output
    2. Recur to change bank of that value in the other half of graph (output or input)
    3. Recur to fix any conflicts that this change added on the other half of the graph

    :param graph: the graph with the banks to modify
    :param conflict: the current conflict to fix
    :param is_input: whether to conflict is on the input or the output nodes of the graph
    :return: the fixed graph
    """
    t = conflict.t
    s = conflict.s
    if is_input:
        graph.input_nodes[t].edge_banks[s] = conflict.new_bank
        output_addr = get_output_address_at_input(t, s, graph.input_type, graph.output_type)
        graph.output_nodes[output_addr.t].edge_banks[output_addr.s] = conflict.new_bank
        induced_conflict = check_conflict(output_addr.t, graph.output_nodes[output_addr.t].edge_banks)
    else:
        graph.output_nodes[t].edge_banks[s] = conflict.new_bank
        input_addr = get_input_address_at_output(t, s, graph.input_type, graph.output_type)
        graph.input_nodes[input_addr.t].edge_banks[input_addr.s] = conflict.new_bank
        induced_conflict = check_conflict(input_addr.t, graph.input_nodes[input_addr.t].edge_banks)
    if induced_conflict:
        resolve_conflict(graph, induced_conflict, not is_input)


def assign_banks(graph: InputOutputGraph) -> InputOutputGraph:
    """
    Given a bipartite graph, fix the bank assignments so that:
    1. The same value is written to and read from the same bank
    2. There are no bank conflicts - each bank is written to or read from on the same clock

    :param graph: The graph to fix
    :return: a fixed graph
    """
    conflict = find_conflict(graph.output_nodes)
    while conflict.is_conflict:
        graph = resolve_conflict(graph, conflict, False)
        conflict = find_conflict(graph.output_nodes)
    return graph
