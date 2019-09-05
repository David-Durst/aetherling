from aetherling.modules.permutation.assign_ts_addresses import *

def test_flat_idxs_tseq_3_0():
    tseq_3_0 = ST_TSeq(3, 0, ST_Int())
    flat_idxs = dimensions_to_flat_idx(tseq_3_0)
    assert flat_idxs == [[FlatIndex(False, 0)],[FlatIndex(False, 1)],[FlatIndex(False, 2)]]

def test_flat_idxs_tseq_3_3():
    tseq_3_0 = ST_TSeq(3, 3, ST_Int())
    flat_idxs = dimensions_to_flat_idx(tseq_3_0)
    assert flat_idxs == [[FlatIndex(False, 0)],[FlatIndex(False, 1)],[FlatIndex(False, 2)],
                         [FlatIndex(True, 0)],[FlatIndex(True, 1)],[FlatIndex(True, 2)]]

def test_flat_idxs_sseq_3():
    sseq_3 = ST_SSeq(3, ST_Int())
    flat_idxs = dimensions_to_flat_idx(sseq_3)
    assert flat_idxs == [[FlatIndex(False, 0), FlatIndex(False, 1), FlatIndex(False, 2)]]

def test_flat_idxs_tseq_2_0_sseq_3():
    vals = ST_TSeq(2, 0, ST_SSeq(3, ST_Int()))
    flat_idxs = dimensions_to_flat_idx(vals)
    assert flat_idxs == [[FlatIndex(False, 0), FlatIndex(False, 1), FlatIndex(False, 2)],
                         [FlatIndex(False, 3), FlatIndex(False, 4), FlatIndex(False, 5)]]

def test_flat_idxs_tseq_2_1_sseq_3():
    vals = ST_TSeq(2, 1, ST_SSeq(3, ST_Int()))
    flat_idxs = dimensions_to_flat_idx(vals)
    assert flat_idxs == [[FlatIndex(False, 0), FlatIndex(False, 1), FlatIndex(False, 2)],
                         [FlatIndex(False, 3), FlatIndex(False, 4), FlatIndex(False, 5)],
                         [FlatIndex(True, 0), FlatIndex(True, 1), FlatIndex(True, 2)]]

def test_flat_idxs_tseq_2_0_tseq_1_1():
    vals = ST_TSeq(2, 0, ST_TSeq(1, 1, ST_Int()))
    flat_idxs = dimensions_to_flat_idx(vals)
    assert flat_idxs == [[FlatIndex(False, 0)], [FlatIndex(True, 0)], [FlatIndex(False, 1)], [FlatIndex(True, 1)]]

def test_flat_idxs_tseq_2_2():
    vals = ST_TSeq(2, 2, ST_Int())
    flat_idxs = dimensions_to_flat_idx(vals)
    assert flat_idxs == [[FlatIndex(False, 0)], [FlatIndex(False, 1)], [FlatIndex(True, 0)], [FlatIndex(True, 1)]]

def test_flat_idxs_sseq_3_tseq_2_1():
    vals = ST_SSeq(3, ST_TSeq(2, 1, ST_Int()))
    flat_idxs = dimensions_to_flat_idx(vals)
    assert flat_idxs == [[FlatIndex(False, 0), FlatIndex(False, 2), FlatIndex(False, 4)],
                         [FlatIndex(False, 1), FlatIndex(False, 3), FlatIndex(False, 5)],
                         [FlatIndex(True, 0), FlatIndex(True, 1), FlatIndex(True, 2)]]

def test_flat_idxs_tseq_2_0_sseq_3_tseq_2_0():
    vals = ST_TSeq(2, 0, ST_SSeq(3, ST_TSeq(2, 0, ST_Int())))
    flat_idxs = dimensions_to_flat_idx(vals)
    assert flat_idxs == [[FlatIndex(False, 0), FlatIndex(False, 2), FlatIndex(False, 4)],
                         [FlatIndex(False, 1), FlatIndex(False, 3), FlatIndex(False, 5)],
                         [FlatIndex(False, 6), FlatIndex(False, 8), FlatIndex(False, 10)],
                         [FlatIndex(False, 7), FlatIndex(False, 9), FlatIndex(False, 11)]]

def test_input_addr_to_output_addr_flip():
    input_type = ST_TSeq(3, 0, ST_SSeq(2, ST_Int()))
    output_type = ST_SSeq(2, ST_TSeq(3, 0, ST_Int()))
    input_non_nested_ts = dimensions_to_flat_idx(input_type)
    output_non_nested_ts = dimensions_to_flat_idx(output_type)
    for t in range(3):
        for s in range(2):
            output_addr = get_output_address_at_input(t, s, input_type, output_type)
            assert input_non_nested_ts[t][s] == output_non_nested_ts[output_addr.t][output_addr.s]

def test_input_addr_to_output_addr_flip_invalids():
    input_type = ST_TSeq(3, 1, ST_SSeq(2, ST_Int()))
    output_type = ST_SSeq(2, ST_TSeq(3, 1, ST_Int()))
    input_non_nested_ts = dimensions_to_flat_idx(input_type)
    output_non_nested_ts = dimensions_to_flat_idx(output_type)
    for t in range(3):
        for s in range(2):
            output_addr = get_output_address_at_input(t, s, input_type, output_type)
            assert input_non_nested_ts[t][s] == output_non_nested_ts[output_addr.t][output_addr.s]
            if output_addr.flat_idx.invalid:
                assert output_addr.flat_idx.idx[0] == t
                assert output_addr.flat_idx.idx[1] == s
