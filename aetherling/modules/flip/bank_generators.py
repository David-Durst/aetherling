from mantle.common.countermod import SizedCounterModM
from mantle.common.operator import *
from aetherling.modules.term_any_type import TermAnyType
from aetherling.modules.ram_any_type import DefineRAMAnyType
from aetherling.modules.register_any_type import DefineRegisterAnyType
from aetherling.modules.mux_any_type import DefineMuxAnyType
from magma import *
from magma.bitutils import int2seq
from magma.circuit import DefineCircuitKind
from .hydrate import Dehydrate, Hydrate
from .map_fully_parallel_sequential import MapParallel
from ..helpers.nameCleanup import cleanName
from ..helpers.magma_helpers import ready_valid_interface
from mantle.coreir.memory import getRAMAddrWidth
from mantle.common.countermod import SizedCounterModM, DefineCounterModM
from mantle.coreir.arith import DefineAdd, DefineUDiv, DefineUMod
from mantle import Decode
from math import gcd

@cache_definition
def DefineTSBankGenerator(no: int, io: int, ni: int, time_per_element: int, T: Kind, has_ce=False, has_reset=False):
    """
    For a TSeq no io (SSeq ni T) input or output to a flip, get the address for reading and writing each T input
    every clock.

    Note that the T passed to this operator just the Magma type each clock cycle.
    You can get T by calling magma_repr on a space-time type T'.

    bank : Out(Array[ni, Array[getRAMAddrWidth(ni), Bit]) - getRAMAddrWidth is roughly log2(ni-1) with adjustment for corner cases
    if has_ce:
    CE : In(Bit)
    if has_reset:
    RESET : In(Bit)
    """
    class _TSBankGenerator(Circuit):
        addr_width = getRAMAddrWidth(ni)

        name = "TSBankGenerator_no{}_io{}_ni{}_tEl{}_T{}_hasCE{}_hasReset{}".format(str(no), str(io), str(ni),
                                                                                    str(time_per_element),
                                                                                    cleanName(str(T)), str(has_ce), str(has_reset))
        IO = ['bank', Out(Array[ni, Array[addr_width, Bit]])] + ClockInterface(has_ce, has_reset)
        @classmethod
        def definition(TSBankGenerator):
            # each element of the SSeq is a separate vector lane
            # increase by time_per_element
            index_in_cur_element = SizedCounterModM(time_per_element, has_ce=has_ce, has_reset=has_reset)
            first_lane_flat_idx = SizedCounterModM(no*ni, incr=ni, has_ce=True, has_reset=has_reset)()
            next_element = Decode(time_per_element - 1, index_in_cur_element.O.N)(index_in_cur_element.O)
            wire(next_element.O, first_lane_flat_idx.CE)
            if has_ce:
                wire(TSBankGenerator.CE, index_in_cur_element.CE)
            if has_reset:
                wire(TSBankGenerator.RESET, index_in_cur_element.RESET)
                wire(TSBankGenerator.RESET, first_lane_flat_idx.RESET)

            lane_flat_idxs = [first_lane_flat_idx.O]

            # compute the current flat_idx for each lane
            for i in range(1,ni):
                cur_lane_flat_idx_adder = DefineAdd(TSBankGenerator.addr_width)()
                wire(cur_lane_flat_idx_adder.I0, first_lane_flat_idx.O)
                wire(cur_lane_flat_idx_adder.I1, int2seq(i, TSBankGenerator.addr_width))

                lane_flat_idxs += [cur_lane_flat_idx_adder.O]

            lane_flat_div_lcms = []
            # conmpute flat_idx / lcm_dim for each lane
            for i in range(0,ni):
                cur_lane_lcm_div = DefineUDiv(TSBankGenerator.addr_width)()
                wire(cur_lane_lcm_div.I0, lane_flat_idxs[0].O)
                wire(cur_lane_lcm_div.I1, int2seq(lcm(no, ni), TSBankGenerator.addr_width))

                lane_flat_div_lcms += [cur_lane_flat_idx_adder.O]

            # compute ((flat_idx % sseq_dim) + (flat_idx / lcm_dim)) % sseq_dim for each lane
            # only need to mod sseq_dim at end as that is same as also doing it flat_idx before addition
            for i in range(0, ni):
                pre_mod_add = DefineAdd(TSBankGenerator.addr_width)()
                wire(pre_mod_add.I0, lane_flat_idxs[i])
                wire(pre_mod_add.I1, lane_flat_div_lcms[i])

                bank_mod = DefineUMod(TSBankGenerator.addr_width)()
                wire(bank_mod.I0, pre_mod_add.O)
                wire(bank_mod.I0, int2seq(no, TSBankGenerator.addr_width))

                wire(TSBankGenerator.O[i], bank_mod.O)

    return _TSBankGenerator


def lcm(a, b):
    return abs(a*b) // gcd(a, b)