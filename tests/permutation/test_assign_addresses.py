from aetherling.space_time_modules.space_time_types import *
from aetherling.modules.permutation.assign_addresses import *

def test_flat_idxs_tseq_3_0():
    tseq_3_0 = ST_TSeq(3, 0, ST_Int())
    flat_idxs = dimensions_to_flat_idx(tseq_3_0)
    assert flat_idxs == [[0],[1],[2]]

def test_flat_idxs_sseq_3():
    sseq_3 = ST_SSeq(3, ST_Int())
    flat_idxs = dimensions_to_flat_idx(sseq_3)
    assert flat_idxs == [[0, 1, 2]]

def test_flat_idxs_tseq_2_0_sseq_3():
    vals = ST_TSeq(2, 0, ST_SSeq(3, ST_Int()))
    flat_idxs = dimensions_to_flat_idx(vals)
    assert flat_idxs == [[0, 1, 2], [3,4,5]]

def test_flat_idxs_tseq_2_0_sseq_3_tseq_2_0():
    vals = ST_TSeq(2, 0, ST_SSeq(3, ST_TSeq(2, 0, ST_Int())))
    flat_idxs = dimensions_to_flat_idx(vals)
    assert flat_idxs == [[0, 2, 4], [1, 3, 5], [6, 8, 10], [7, 9, 11]]
