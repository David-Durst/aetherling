from aetherling.modules.flip.bank_addr_generators import DefineSTBankAddrGenerator, DefineTSBankAddrGenerator, lcm
from magma import *
from magma.bitutils import *
from aetherling.helpers.fault_helpers import compile_and_run
import fault
import random
from aetherling.modules.term_any_type import TermAnyType
from mantle.coreir import DefineCoreirConst

def ts_lens_to_vals(t_len, s_len):
    return [[t*s_len + x for x in range(s_len)] for t in range(t_len)]

def st_lens_to_vals(s_len, t_len):
    return [[x*t_len + t for t in range(t_len)] for x in range(s_len)]


#SSeq (TSeq) bank and address computations
#bank = ((flat_idx % sseq_dim) + (flat_idx / lcm_dim)) % sseq_dim
#addr = flat_idx / sseq_dim
def test_sseq2tseq3_0_bank_addr_generator():
    width = 11
    T = Array[width, BitIn]
    sseq_dim = 2
    tseq_dim = 3
    lcm_dim = lcm(sseq_dim, tseq_dim)
    st_flat_idxs = st_lens_to_vals(sseq_dim, tseq_dim)
    # transpose the st_flat_idxs so inner array is on same clock
    flat_idxs_inner_per_clock = [[st_flat_idxs[s][t] for s in range(sseq_dim)] for t in range(tseq_dim)]
    # banks and addr should write to each clock
    banks_per_clock = [[((flat_idx % sseq_dim) + (flat_idx // lcm_dim)) % sseq_dim for flat_idx in flat_idxs_one_clock] for flat_idxs_one_clock in flat_idxs_inner_per_clock]
    addr_per_clock = [[flat_idx // sseq_dim for flat_idx in flat_idxs_one_clock] for flat_idxs_one_clock in flat_idxs_inner_per_clock]

    testcircuit = DefineSTBankAddrGenerator(sseq_dim, tseq_dim, 0, 1)

    tester = fault.Tester(testcircuit, testcircuit.CLK)

    for t in range(tseq_dim):
        tester.eval()
        for s in range(sseq_dim):
            tester.circuit.bank.expect(banks_per_clock[t][s])
            tester.circuit.addr.expect(addr_per_clock[t][s])
        tester.step(2)

    compile_and_run(tester)

