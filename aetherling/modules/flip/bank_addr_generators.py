from mantle.common.countermod import SizedCounterModM
from mantle.common.operator import *
from aetherling.modules.term_any_type import TermAnyType
from aetherling.modules.ram_any_type import DefineRAMAnyType
from aetherling.modules.register_any_type import DefineRegisterAnyType
from aetherling.modules.mux_any_type import DefineMuxAnyType
from magma import *
from magma.circuit import DefineCircuitKind
from aetherling.modules.hydrate import Dehydrate, Hydrate
from aetherling.modules.map_fully_parallel_sequential import MapParallel
from aetherling.helpers.nameCleanup import cleanName
from aetherling.helpers.magma_helpers import ready_valid_interface
from mantle.coreir.memory import getRAMAddrWidth
from mantle.common.countermod import SizedCounterModM, DefineCounterModM
from mantle.coreir.arith import DefineAdd, DefineUDiv, DefineUMod
from mantle import Decode
from math import gcd
from mantle.coreir import DefineCoreirConst



@cache_definition
def DefineTSBankAddrGenerator(no: int, io: int, ni: int, time_per_element: int, has_ce=False, has_reset=False):
    """
    For a TSeq no io (SSeq ni T) input to or output from a flip, get the bank and address for reading and writing each T input
    every clock.

    You can get the time_per_element by calling time() on a space-time type T'.

    TSeq (SSeq) bank and address computations
    bank = (s + (flat_idx / lcm_dim)) % sseq_dim
    addr = t

    bank : Out(Array[ni, Array[getRAMAddrWidth(ni), Bit]) - getRAMAddrWidth is roughly log2(ni-1) with adjustment for corner cases
    addr : Out(Array[ni, Array[getRAMAddrWidth(no), Bit])
    if has_ce:
    CE : In(Bit)
    if has_reset:
    RESET : In(Bit)
    """
    class _TSBankGenerator(Circuit):
        bank_width = getRAMAddrWidth(ni)
        addr_width = getRAMAddrWidth(no)

        name = "TSBankGenerator_no{}_io{}_ni{}_tEl{}_T{}_hasCE{}_hasReset{}".format(str(no), str(io), str(ni),
                                                                                    str(time_per_element),
                                                                                    str(has_ce), str(has_reset))
        IO = ['bank', Array[no, Array[bank_width, Bit]], 'addr', Array[no, Array[addr_width, Bit]]] + \
             ClockInterface(has_ce, has_reset)
        @classmethod
        def definition(TSBankGenerator):
            flat_idx_width = getRAMAddrWidth(no*ni)
            # next element each time_per_element clock
            if time_per_element > 1:
                index_in_cur_element = SizedCounterModM(time_per_element, has_ce=has_ce, has_reset=has_reset)
                next_element = Decode(time_per_element - 1, index_in_cur_element.O.N)(index_in_cur_element.O)
            else:
                next_element = DefineCoreirConst(1,1)()
            # each element of the SSeq is a separate vector lane
            first_lane_flat_idx = SizedCounterModM((no+io)*ni, incr=ni, has_ce=True, has_reset=has_reset)()
            time_counter = SizedCounterModM(no+io, has_ce=True, has_reset=has_reset)
            wire(next_element.O, first_lane_flat_idx.CE)
            wire(next_element.O, time_counter.CE)
            if has_ce:
                wire(TSBankGenerator.CE, index_in_cur_element.CE)
            if has_reset:
                wire(TSBankGenerator.RESET, index_in_cur_element.RESET)
                wire(TSBankGenerator.RESET, first_lane_flat_idx.RESET)
                wire(TSBankGenerator.RESET, time_counter.RESET)

            lane_flat_idxs = [first_lane_flat_idx.O]

            # compute the current flat_idx for each lane
            for i in range(1,ni):
                cur_lane_flat_idx_adder = DefineAdd(flat_idx_width)()
                wire(cur_lane_flat_idx_adder.I0, first_lane_flat_idx.O)
                wire(cur_lane_flat_idx_adder.I1, DefineCoreirConst(flat_idx_width, i*no)().O)

                lane_flat_idxs += [cur_lane_flat_idx_adder.O]

            lane_flat_div_lcms = []
            # conmpute flat_idx / lcm_dim for each lane
            for i in range(ni):
                cur_lane_lcm_div = DefineUDiv(flat_idx_width)()
                wire(cur_lane_lcm_div.I0, lane_flat_idxs[0].O)
                wire(cur_lane_lcm_div.I1, DefineCoreirConst(lcm(no, ni), flat_idx_width)().O)

                lane_flat_div_lcms += [cur_lane_flat_idx_adder.O]

            # compute ((flat_idx % sseq_dim) + (flat_idx / lcm_dim)) % sseq_dim for each lane
            # note that s_ts == flat_idx % sseq_dim
            # only need to mod sseq_dim at end as that is same as also doing it flat_idx before addition
            for i in range(ni):
                pre_mod_add = DefineAdd(flat_idx_width)()
                wire(pre_mod_add.I0, lane_flat_idxs[i])
                wire(pre_mod_add.I1, lane_flat_div_lcms[i])

                bank_mod = DefineUMod(flat_idx_width)()
                wire(bank_mod.I0, pre_mod_add.O)
                wire(bank_mod.I0, DefineCoreirConst(flat_idx_width, ni)().O)

                wire(TSBankGenerator.bank[i], bank_mod.O[0:TSBankGenerator.bank_width])

            # compute t for each lane addr
            for i in range(0,ni):
                wire(TSBankGenerator.addr[i], time_counter.O[0:TSBankGenerator.addr_width])

    return _TSBankGenerator

@cache_definition
def DefineSTBankAddrGenerator(no: int, ni: int, ii: int, time_per_element: int, has_ce=False, has_reset=False):
    """
    For a SSeq no (TSeq ni ii T) input to or output from a flip, get the bank and address for reading and writing each T input
    every clock.

    You can get the time_per_element by calling time() on a space-time type T'.

    This counter will emit during the ii invalid periods. The rest of the circuit must know to ignore it during those
    periods.

    SSeq (TSeq) bank and address computations
    bank = ((flat_idx % sseq_dim) + (flat_idx / lcm_dim)) % sseq_dim
    addr = flat_idx / sseq_dim

    bank : Out(Array[ni, Array[getRAMAddrWidth(no), Bit]) - getRAMAddrWidth is roughly log2(ni-1) with adjustment for corner cases
    addr : Out(Array[ni, Array[getRAMAddrWidth(ni), Bit])
    if has_ce:
    CE : In(Bit)
    if has_reset:
    RESET : In(Bit)
    """
    class _STBankGenerator(Circuit):
        bank_width = getRAMAddrWidth(no)
        addr_width = getRAMAddrWidth(ni)
        name = "STBankGenerator_no{}_ni{}_ii{}_tEl{}_hasCE{}_hasReset{}".format(str(no), str(ni), str(ii),
                                                                                    str(time_per_element),
                                                                                    str(has_ce), str(has_reset))
        IO = ['bank', Out(Array[no, Array[bank_width, Bit]]), 'addr', Out(Array[no, Array[addr_width, Bit]])] + \
             ClockInterface(has_ce, has_reset)
        @classmethod
        def definition(STBankGenerator):
            flat_idx_width = getRAMAddrWidth(no*ni)
            # next element each time_per_element clock
            if time_per_element > 1:
                index_in_cur_element = SizedCounterModM(time_per_element, has_ce=has_ce, has_reset=has_reset)
                next_element = Decode(time_per_element - 1, index_in_cur_element.O.N)(index_in_cur_element.O)
            else:
                next_element = DefineCoreirConst(1,1)()
            # each element of the SSeq is a separate vector lane
            first_lane_flat_idx = DefineCounterModM(ni+ii, flat_idx_width, cout=False, has_ce=True, has_reset=has_reset)()
            wire(next_element.O[0], first_lane_flat_idx.CE)
            if has_ce:
                wire(STBankGenerator.CE, index_in_cur_element.CE)
            if has_reset:
                wire(STBankGenerator.RESET, index_in_cur_element.RESET)
                wire(STBankGenerator.RESET, first_lane_flat_idx.RESET)

            lane_flat_idxs = [first_lane_flat_idx.O]

            # compute the current flat_idx for each lane
            for i in range(1,no):
                cur_lane_flat_idx_adder = DefineAdd(flat_idx_width)()
                wire(cur_lane_flat_idx_adder.I0, first_lane_flat_idx.O)
                wire(cur_lane_flat_idx_adder.I1, DefineCoreirConst(flat_idx_width, i*ni)().O)

                lane_flat_idxs += [cur_lane_flat_idx_adder.O]

            lane_flat_div_lcms = []
            lcm_dim = DefineCoreirConst(flat_idx_width, lcm(no, ni))()
            # conmpute flat_idx / lcm_dim for each lane
            for i in range(no):
                cur_lane_lcm_div = DefineUDiv(flat_idx_width)()
                wire(cur_lane_lcm_div.I0, lane_flat_idxs[i])
                wire(cur_lane_lcm_div.I1, lcm_dim.O)

                lane_flat_div_lcms += [cur_lane_lcm_div.O]

            # compute ((flat_idx % sseq_dim) + (flat_idx / lcm_dim)) % sseq_dim for each lane
            # only need to mod sseq_dim at end as that is same as also doing it flat_idx before addition
            for i in range(no):
                pre_mod_add = DefineAdd(flat_idx_width)()
                wire(pre_mod_add.I0, lane_flat_idxs[i])
                wire(pre_mod_add.I1, lane_flat_div_lcms[i])

                bank_mod = DefineUMod(flat_idx_width)()
                wire(bank_mod.I0, pre_mod_add.O)
                wire(bank_mod.I1, DefineCoreirConst(flat_idx_width, no)().O)

                wire(STBankGenerator.bank[i], bank_mod.O[0:STBankGenerator.bank_width])
                if len(bank_mod.O) > STBankGenerator.bank_width:
                    bits_to_term = len(bank_mod.O) - STBankGenerator.bank_width
                    term = TermAnyType(Array[bits_to_term, Bit])
                    wire(bank_mod.O[STBankGenerator.bank_width:], term.I)

            # compute flat_idx / sseq_dim for each lane addr
            for i in range(no):
                flat_idx_sseq_dim_div = DefineUDiv(flat_idx_width)()
                wire(flat_idx_sseq_dim_div.I0, lane_flat_idxs[0])
                wire(flat_idx_sseq_dim_div.I1, DefineCoreirConst(flat_idx_width, no)().O)

                wire(STBankGenerator.addr[i], flat_idx_sseq_dim_div.O[0:STBankGenerator.addr_width])
                if len(flat_idx_sseq_dim_div.O) > STBankGenerator.addr_width:
                    bits_to_term = len(bank_mod.O) - STBankGenerator.addr_width
                    term = TermAnyType(Array[bits_to_term, Bit])
                    wire(flat_idx_sseq_dim_div.O[STBankGenerator.addr_width:], term.I)

    return _STBankGenerator

def lcm(a, b):
    return abs(a*b) // gcd(a, b)