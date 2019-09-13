from aetherling.space_time.space_time_types import *
from aetherling.space_time.nested_counters import *
from aetherling.space_time.ram_st import DefineRAM_ST
from aetherling.space_time.type_helpers import get_shared_and_diff_subtypes, remove_tseqs, replace_tombstone, \
    ST_Tombstone, num_nested_layers
from aetherling.modules.term_any_type import TermAnyType
from aetherling.modules.flip.bitonic_sort import DefineBitonicSort
from aetherling.modules.permutation import build_permutation_graph, BipartiteNode, get_output_latencies
from aetherling.modules.initial_delay_counter import DefineInitialDelayCounter
from aetherling.modules.lut_any_type import DefineLUTAnyType
from aetherling.modules.counter import AESizedCounterModM
from aetherling.helpers.nameCleanup import cleanName
from mantle.coreir.memory import getRAMAddrWidth
from mantle.common.countermod import Decode, SizedCounterModM
from mantle.coreir import DefineCoreirConst
from aetherling.modules.ram_any_type import *
from magma import *
from magma.bitutils import int2seq
from magma.circuit import DefineCircuitKind, Circuit
from typing import List
import builtins
import numpy as np

__all__ = ['DefineReshape_ST', 'Reshape_ST']

@dataclass(order=True, frozen=True)
class LaneToBankPerClock:
    """
    The mapping from an input or output vector lane to a bank for one clock cycle.
    """
    s: int
    t: int
    bank: int
    addr: int
    valid: bool


def get_banks_addr_per_lane(graph_nodes: List[BipartiteNode]) -> List[List[LaneToBankPerClock]]:
    """
    Get the banks and addr per lane per clock for either the input_nodes or output_nodes of a permutation graph
    :param graph_nodes: Either the input_nodes or output_nodes of a permutation graph
    :return: Outer dimension is indexed by lane, inner dimension is indexed by clock cycle
    """
    num_clocks = len(graph_nodes)
    num_lanes = len(graph_nodes[0].edge_banks)
    lane_bank_data = [[] for _ in range(num_lanes)]
    for s in range(num_lanes):
        for t in range(num_clocks):
            lane_bank_data[s].append(LaneToBankPerClock(s, t, graph_nodes[t].edge_banks[s],
                                                    graph_nodes[t].edge_addr[s], not graph_nodes[t].flat_idxs[s].invalid))
    return lane_bank_data


def get_lane_addr_per_banks(graph_nodes: List[BipartiteNode]) -> List[List[LaneToBankPerClock]]:
    """
    Get the lane and addr per bank per clock for either the input_nodes or output_nodes of a permutation graph
    :param graph_nodes: Either the input_nodes or output_nodes of a permutation graph
    :return: Outer dimension is indexed by bank, inner dimension is indexed by clock cycle
    """
    num_clocks = len(graph_nodes)
    num_banks = len(graph_nodes[0].edge_banks)
    # note: lanes include invalid lanes used to balance out input and output port widths
    num_lanes = num_banks
    lane_bank_data = [[None for _ in range(num_clocks)] for _ in range(num_banks)]
    for s in range(num_lanes):
        for t in range(num_clocks):
            bank = graph_nodes[t].edge_banks[s]
            lane_bank_data[bank][t] = LaneToBankPerClock(s, t, bank, graph_nodes[t].edge_addr[s],
                                                           not graph_nodes[t].flat_idxs[s].invalid)
    return lane_bank_data

def transpose_outer_dimensions(outer_dimensions: ST_Type, diff_dimensions: ST_Type, inner_dimensions: ST_Type,
                               ports_to_transpose: List) -> Kind:
    """
    Transpose the outer dimensions of a set of ports, move them inside the diff dimensions. The outer dimensions
    that are sseqs are the same for all elements, so treat as inner dimensions.
    :param outer_dimensions: The outer dimensions that need to be moved inside
    :param diff_dimensions: The dimensions that need to be moved outside
    :param inner_dimensions: The inner dimensions that need to stay where they are
    :param ports_to_transpose: The ports
    :return:
    """
    # always remove tseqs as they don't affect the magma types
    num_outer_dimensions = num_nested_layers(remove_tseqs(outer_dimensions))
    num_diff_dimensions = num_nested_layers(remove_tseqs(diff_dimensions))
    num_inner_dimensions = num_nested_layers(remove_tseqs(inner_dimensions))
    orig_arr = np.asarray(ports_to_transpose)

    # these are the indexes of the dimensions on the untransposed type
    outer_dimensions_indexes_untransposed = list(range(num_outer_dimensions))
    diff_dimensions_indexes_untransposed = list(range(num_outer_dimensions, num_outer_dimensions + num_diff_dimensions))
    inner_dimensions_indexes_untransposed = list(range(num_outer_dimensions + num_diff_dimensions,
                                                       num_outer_dimensions + num_diff_dimensions + num_inner_dimensions))

    transposed_arr = orig_arr.transpose(diff_dimensions_indexes_untransposed + outer_dimensions_indexes_untransposed +
                                        inner_dimensions_indexes_untransposed)
    return transposed_arr.tolist()

def flatten_ports(ports_to_flatten: List) -> List:
    """
    Convert a nested list of ports to a flat list of ports
    """
    ports_arr = np.asarray(ports_to_flatten)
    flattened_arr = ports_arr.flatten()
    return flattened_arr.tolist()

@cache_definition
def DefineReshape_ST(t_in: ST_Type, t_out: ST_Type, has_ce=False, has_reset=False) -> DefineCircuitKind:
    """
    Convert between two space-time types of the same throughput

    I : In(t_in.magma_repr()),
    O : Out(t_out.magma_repr()),

    if has_ce:
    CE : In(Bit)
    if has_reset:
    RESET : In(Bit)

    Note: property output_delay indicates delay of output relative to input
    """

    class _Reshape_ST(Circuit):
        name = 'Reshape_ST{}_{}_hasCE{}_hasReset{}'.format(cleanName(str(t_in)), cleanName(str(t_out)),
                                                           str(has_ce), str(has_reset))
        name = 'testy_namer'
        IO = ['I', In(t_in.magma_repr()),
              'O', Out(t_out.magma_repr())
              #'ram_wr', Out(t_in.magma_repr()),
              #'addr_wr', Out(Array[2, Array[1, Bit]]),
              #'ram_rd', Out(t_in.magma_repr()),
              #'addr_rd', Out(Array[2, Array[1, Bit]]),
              #'reshape_write_counter', Out(Array[2, Bit]),
              #'first_valid', Out(Bit)
              ] + ClockInterface(has_ce=has_ce, has_reset=has_reset)
        @classmethod
        def definition(cls):
            # first section creates the RAMs and LUTs that set values in them and the sorting network
            shared_and_diff_subtypes = get_shared_and_diff_subtypes(t_in, t_out)
            t_in_diff = shared_and_diff_subtypes.diff_input
            t_out_diff = shared_and_diff_subtypes.diff_output
            graph = build_permutation_graph(t_in_diff, t_out_diff)
            banks_write_addr_per_input_lane = get_banks_addr_per_lane(graph.input_nodes)
            input_lane_write_addr_per_bank = get_lane_addr_per_banks(graph.input_nodes)
            output_lane_read_addr_per_bank = get_lane_addr_per_banks(graph.output_nodes)

            # each ram only needs to be large enough to handle the number of addresses assigned to it
            # all rams receive the same number of writes
            # but some of those writes don't happen as the data is invalid, so don't need storage for them
            max_ram_addrs = [max([bank_clock_data.addr for bank_clock_data in bank_data])
                               for bank_data in output_lane_read_addr_per_bank]
            # rams also handle parallelism from outer_shared type as this affects all banks the same
            outer_shared_sseqs = remove_tseqs(shared_and_diff_subtypes.shared_outer)
            if outer_shared_sseqs == ST_Tombstone():
                ram_element_type = shared_and_diff_subtypes.shared_inner
            else:
                ram_element_type = replace_tombstone(outer_shared_sseqs, shared_and_diff_subtypes.shared_inner)
            rams = [DefineRAM_ST(ram_element_type, ram_max_addr + 1)() for ram_max_addr in max_ram_addrs]
            rams_addr_widths = [ram.WADDR.N for ram in rams]

            # for bank, the addresses to write to each clock
            write_addr_for_bank_luts = []
            for bank_idx in range(len(rams)):
                ram_addr_width = rams_addr_widths[bank_idx]
                num_addrs = len(input_lane_write_addr_per_bank[bank_idx])
                assert num_addrs == t_in_diff.time()
                addrs = [builtins.tuple(int2seq(write_data_per_bank_per_clock.addr, ram_addr_width))
                         for write_data_per_bank_per_clock in input_lane_write_addr_per_bank[bank_idx]]
                write_addr_for_bank_luts.append(
                    DefineLUTAnyType(Array[ram_addr_width, Bit], num_addrs, builtins.tuple(addrs))())

            # for bank, whether to actually write this clock
            write_valid_for_bank_luts = []
            for bank_idx in range(len(rams)):
                num_valids = len(input_lane_write_addr_per_bank[bank_idx])
                assert num_valids == t_in_diff.time()
                valids = [builtins.tuple([write_data_per_bank_per_clock.valid])
                         for write_data_per_bank_per_clock in input_lane_write_addr_per_bank[bank_idx]]
                write_valid_for_bank_luts.append(
                    DefineLUTAnyType(Bit, num_valids, builtins.tuple(valids))())

            # for each input lane, the bank to write to each clock
            write_bank_for_input_lane_luts = []
            bank_idx_width = getRAMAddrWidth(len(rams))
            for lane_idx in range(len(banks_write_addr_per_input_lane)):
                num_bank_idxs = len(banks_write_addr_per_input_lane[lane_idx])
                assert num_bank_idxs == t_in_diff.time()
                bank_idxs = [builtins.tuple(int2seq(write_data_per_lane_per_clock.bank, bank_idx_width))
                             for write_data_per_lane_per_clock in banks_write_addr_per_input_lane[lane_idx]]
                write_bank_for_input_lane_luts.append(
                    DefineLUTAnyType(Array[bank_idx_width, Bit], num_bank_idxs, builtins.tuple(bank_idxs))())

            # for each bank, the address to read from each clock
            read_addr_for_bank_luts = []
            for bank_idx in range(len(rams)):
                ram_addr_width = rams_addr_widths[bank_idx]
                num_addrs = len(output_lane_read_addr_per_bank[bank_idx])
                assert num_addrs == t_in_diff.time()
                addrs = [builtins.tuple(int2seq(read_data_per_bank_per_clock.addr, ram_addr_width))
                         for read_data_per_bank_per_clock in output_lane_read_addr_per_bank[bank_idx]]
                read_addr_for_bank_luts.append(
                    DefineLUTAnyType(Array[ram_addr_width, Bit], num_addrs, builtins.tuple(addrs))())

            # for each bank, the lane to send each read to
            output_lane_for_bank_luts = []
            # number of lanes equals number of banks
            # some the lanes are just always invalid, added so input lane width equals output lane width
            lane_idx_width = getRAMAddrWidth(len(rams))
            for bank_idx in range(len(rams)):
                num_lane_idxs = len(output_lane_read_addr_per_bank[bank_idx])
                assert num_lane_idxs == t_in_diff.time()
                lane_idxs = [builtins.tuple(int2seq(read_data_per_bank_per_clock.s, lane_idx_width))
                             for read_data_per_bank_per_clock in output_lane_read_addr_per_bank[bank_idx]]
                output_lane_for_bank_luts.append(
                    DefineLUTAnyType(Array[lane_idx_width, Bit], num_lane_idxs, builtins.tuple(lane_idxs))())


            # second part creates the counters that index into the LUTs
            # elem_per counts time per element of the reshape
            elem_per_reshape_counter = AESizedCounterModM(ram_element_type.time(), has_ce=has_ce)
            end_cur_elem = Decode(ram_element_type.time() - 1,
                                  elem_per_reshape_counter.O.N)(elem_per_reshape_counter.O)
            # reshape counts which element in the reshape
            reshape_write_counter = AESizedCounterModM(t_in_diff.time(), has_ce=True, has_reset=has_reset)
            reshape_read_counter = AESizedCounterModM(t_in_diff.time(), has_ce=True, has_reset=has_reset)

            output_delay = (get_output_latencies(graph)[0]) * ram_element_type.time()
            # this is present so testing knows the delay
            cls.output_delay = output_delay
            reshape_read_delay_counter = DefineInitialDelayCounter(output_delay, has_ce=has_ce, has_reset=has_reset)()
            # outer counter the repeats the reshape
            repeat_reshape_counter = DefineNestedCounters(shared_and_diff_subtypes.shared_outer, has_last=False,
                                                          has_ce=has_ce, has_reset=has_reset)()
            #wire(reshape_write_counter.O, cls.reshape_write_counter)

            if has_ce:
                wire(cls.CE, elem_per_reshape_counter.CE)
                wire(cls.CE, reshape_read_delay_counter.CE)
                wire(cls.CE, repeat_reshape_counter.CE)
                wire(bit(cls.CE) & repeat_reshape_counter.valid & end_cur_elem, reshape_write_counter.CE)
                wire(bit(cls.CE) & repeat_reshape_counter.valid & end_cur_elem & reshape_read_delay_counter.valid,
                     reshape_read_counter.CE)
            else:
                wire(repeat_reshape_counter.valid & end_cur_elem, reshape_write_counter.CE)
                wire(repeat_reshape_counter.valid & end_cur_elem & reshape_read_delay_counter.valid, reshape_read_counter.CE)

            if has_reset:
                wire(cls.RESET, elem_per_reshape_counter.RESET)
                wire(cls.RESET, reshape_read_delay_counter.RESET)
                wire(cls.RESET, repeat_reshape_counter.RESET)
                wire(cls.RESET, reshape_write_counter.RESET)
                wire(cls.RESET, reshape_read_counter.RESET)


            # wire read and write counters to all LUTs
            for lut in write_bank_for_input_lane_luts:
                wire(reshape_write_counter.O, lut.addr)

            for lut in write_addr_for_bank_luts:
                wire(reshape_write_counter.O, lut.addr)

            for lut in write_valid_for_bank_luts:
                wire(reshape_write_counter.O, lut.addr)

            for lut in read_addr_for_bank_luts:
                wire(reshape_read_counter.O, lut.addr)

            for lut in output_lane_for_bank_luts:
                wire(reshape_read_counter.O, lut.addr)


            # third and final instance creation part creates the sorting networks that map lanes to banks
            input_sorting_network_t = Tuple(
                bank=Array[write_bank_for_input_lane_luts[0].data.N, Bit],
                val=ram_element_type.magma_repr())
            input_sorting_network = DefineBitonicSort(input_sorting_network_t,
                                                      len(rams),
                                                      lambda x: x.bank)()

            output_sorting_network_t = Tuple(
                lane=Array[output_lane_for_bank_luts[0].data.N, Bit],
                val=ram_element_type.magma_repr())
            output_sorting_network = DefineBitonicSort(output_sorting_network_t,
                                                      len(rams),
                                                      lambda x: x.lane)()

            # wire luts, sorting networks, inputs, and rams
            input_ports = flatten_ports(cls.I)
            output_ports = flatten_ports(cls.O)
            for idx in range(len(rams)):
                # wire input and bank to input sorting network
                wire(write_bank_for_input_lane_luts[idx].data, input_sorting_network.I[idx].bank)
                #if idx == 0:
                #    wire(cls.first_valid, write_valid_for_bank_luts[idx].data)
                if idx < t_in_diff.port_width():
                    wire(input_ports[idx], input_sorting_network.I[idx].val)
                    #wire(cls.ram_wr[idx], input_sorting_network.O[idx].val)
                    #wire(cls.ram_rd[idx], rams[idx].RDATA)
                else:
                    wire(DefineCoreirConst(ram_element_type.magma_repr().size(), 0)().O,
                         input_sorting_network.I[idx].val)

                # wire input sorting network, write addr, and write valid luts to banks
                wire(input_sorting_network.O[idx].val, rams[idx].WDATA)
                wire(write_addr_for_bank_luts[idx].data, rams[idx].WADDR)
                #wire(write_addr_for_bank_luts[idx].data, cls.addr_wr[idx])
                if has_ce:
                    wire(write_valid_for_bank_luts[idx].data & bit(cls.CE), rams[idx].WE)
                else:
                    wire(write_valid_for_bank_luts[idx].data, rams[idx].WE)

                # wire output sorting network, read addr, read bank, and read enable
                wire(rams[idx].RDATA, output_sorting_network.I[idx].val)
                wire(output_lane_for_bank_luts[idx].data, output_sorting_network.I[idx].lane)
                wire(read_addr_for_bank_luts[idx].data, rams[idx].RADDR)
                #wire(read_addr_for_bank_luts[idx].data, cls.addr_rd[idx])
                # ok to read invalid things, so in read value LUT
                if has_ce:
                    wire(bit(cls.CE), rams[idx].RE)
                else:
                    wire(DefineCoreirConst(1, 1)().O[0], rams[idx].RE)
                if has_reset:
                    wire(cls.RESET, rams[idx].RESET)

                # wire output sorting network value to output or term
                if idx < t_out_diff.port_width():
                    wire(output_sorting_network.O[idx].val, output_ports[idx])
                else:
                    wire(output_sorting_network.O[idx].val, TermAnyType(type(output_sorting_network.O[idx].val)))

                # wire sorting networks bank/lane to term as not used on outputs, just used for sorting
                wire(input_sorting_network.O[idx].bank, TermAnyType(type(input_sorting_network.O[idx].bank)))
                wire(output_sorting_network.O[idx].lane, TermAnyType(type(output_sorting_network.O[idx].lane)))

    return _Reshape_ST

def Reshape_ST(t_in: ST_Type, t_out: ST_Type, has_ce=False, has_reset=False) -> Circuit:
    DefineReshape_ST(t_in, t_out, has_ce=has_ce, has_reset=has_reset)
