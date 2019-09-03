from aetherling.space_time_modules.space_time_types import *
from aetherling.space_time_modules.type_helpers import *

def test_same_type():
    x = ST_TSeq(3, 0, ST_Int())
    y = ST_TSeq(3, 0, ST_Int())
    shared_diff = get_shared_and_diff_subtypes(x,y)
    assert shared_diff.diff_input == ST_Tombstone()
    assert shared_diff.diff_output == ST_Tombstone()
    assert shared_diff.shared_inner == x
    assert shared_diff.shared_outer == ST_Tombstone()


def test_same_type_nested():
    x = ST_TSeq(3, 0,  ST_SSeq(4, ST_Int))
    y = ST_TSeq(3, 0,  ST_SSeq(4, ST_Int))
    shared_diff = get_shared_and_diff_subtypes(x,y)
    assert shared_diff.diff_input == ST_Tombstone()
    assert shared_diff.diff_output == ST_Tombstone()
    assert shared_diff.shared_inner == x
    assert shared_diff.shared_outer == ST_Tombstone()

def test_diff_no_outer_same():
    x = ST_SSeq(6, ST_TSeq(3, 0,  ST_SSeq(4, ST_Int)))
    y = ST_TSeq(3, 0, ST_SSeq(6, ST_SSeq(4, ST_Int)))
    shared_diff = get_shared_and_diff_subtypes(x,y)
    assert shared_diff.diff_input == ST_SSeq(6, ST_TSeq(3, 0, ST_Tombstone()))
    assert shared_diff.diff_output == ST_TSeq(3, 0, ST_SSeq(6, ST_Tombstone()))
    assert shared_diff.shared_inner == x.t.t
    assert shared_diff.shared_outer == ST_Tombstone()

def test_diff_with_outer_same():
    x = ST_TSeq(9, 2, ST_SSeq(6, ST_TSeq(3, 0,  ST_SSeq(4, ST_Int))))
    y = ST_TSeq(9, 2, ST_TSeq(3, 0, ST_SSeq(6, ST_SSeq(4, ST_Int))))
    shared_diff = get_shared_and_diff_subtypes(x,y)
    assert shared_diff.diff_input == ST_SSeq(6, ST_TSeq(3, 0, ST_Tombstone()))
    assert shared_diff.diff_output == ST_TSeq(3, 0, ST_SSeq(6, ST_Tombstone()))
    assert shared_diff.shared_inner == x.t.t.t
    assert shared_diff.shared_outer == ST_TSeq(9, 2, ST_Tombstone())
