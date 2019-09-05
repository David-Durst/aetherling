from aetherling.space_time_modules.space_time_types import *
from itertools import accumulate, groupby
from functools import reduce
from dataclasses import dataclass
import functools
from typing import List, Union, Dict
import typing

@dataclass(order=True, frozen=True)
class FlatIndex:
    """
    Flat indexes for both invalids and valids.
    If invalid, there is no flat index.
    The goal there is just to align the first invalids temporally rather than preserving flattened order.
    Therefore, flat_idx is just a tuple of t and s. t comes first so sorting them puts t's together.
    """
    invalid: bool
    idx: Union[int, typing.Tuple[int, int]]

@dataclass
class SpaceTimeIndex:
    """
    Class for traccking the flat_idx and space and time indexes of a values
    in a space-time object
    """
    flat_idx: FlatIndex
    s: int
    t: int

@functools.lru_cache(maxsize=None, typed=False)
def get_output_address_at_input(t:int, s:int, input_type, output_type) -> SpaceTimeIndex:
    """
    Given non-nested space-time coordinates in input, get the non-nested
    space-time coordinates for the output
    :param s: non-nested space coordinate in input
    :param t: non-nest time coordinate in output
    :param input_type: input nested Space-Time type
    :param output_type:  output nested Space-Time type
    :return: SpaceTimeIndex non-nested coordinates
    """
    non_nested_input_flat_indexes = dimensions_to_flat_idx(input_type)
    flat_index = non_nested_input_flat_indexes[t][s]
    output_flat_to_st_lookup = get_flat_idx_to_space_time_idx_lookup_table(output_type)
    return output_flat_to_st_lookup[flat_index]

@functools.lru_cache(maxsize=None, typed=False)
def get_input_address_at_output(t:int, s:int, input_type, output_type) -> SpaceTimeIndex:
    """
    Given non-nested space-time coordinates in output, get the non-nested
    space-time coordinates for the inpput
    :param s: non-nested space coordinate in input
    :param t: non-nest time coordinate in output
    :param input_type: input nested Space-Time type
    :param output_type:  output nested Space-Time type
    :return: SpaceTimeIndex with non-nested coordinates
    """
    non_nested_output_flat_indexes = dimensions_to_flat_idx(output_type)
    flat_index = non_nested_output_flat_indexes[t][s]
    input_flat_to_st_lookup = get_flat_idx_to_space_time_idx_lookup_table(input_type)
    return input_flat_to_st_lookup[flat_index]

@functools.lru_cache(maxsize=None, typed=False)
def get_flat_idx_to_space_time_idx_lookup_table(dims) -> Dict[FlatIndex, SpaceTimeIndex]:
    space_time_indexes = dimensions_to_space_time_index(dims)[0]
    return {idx.flat_idx:idx for idx in space_time_indexes}

@functools.lru_cache(maxsize=None, typed=False)
def dimensions_to_flat_idx(dims) -> List[FlatIndex]:
    """
    Convert a nested space-time type into a 2d space-time representation.
    This function tells me when each value should be read and written and if it's valid.
    Each outer dimension of the output array is a clock cycle.
    Each inner dimension of the output array is a vector lane.

    :param dims: the space-time type to convert
    """
    flat_elems = dimensions_to_space_time_index(dims)[0]
    # need to sort before groupby to get actual, sql like groupby
    flattened_sorted = sorted(flat_elems, key=lambda x: x.t)
    flattened_grouped_ts = [list(group) for _, group in groupby(flattened_sorted, lambda x: x.t)]
    # sort by s within each group now
    flattened_ts = list(map(lambda g : sorted(g, key=lambda x: x.s), flattened_grouped_ts))
    return list(map(lambda l : list(map(lambda x : x.flat_idx, l)), flattened_ts))

def fix_invalid_indexes(indexes: List[SpaceTimeIndex]) -> SpaceTimeIndex:
    """
    Given a list of SpaceTimeIndexes, convert all the invalid indexs' FlatIndexes idx from (t,s) to
    ints that represent their order in time rather than flattened order.
    :param indexes: indexes that need their invalids converted from (t,s) to ints
    :return: the indexes after conversion
    """
    sorted_invalid_indexes = sorted([orig_idx.flat_idx.idx for orig_idx in indexes if orig_idx.flat_idx.invalid])
    invalid_ts_to_flat_idx = {}
    for i in range(len(sorted_invalid_indexes)):
        invalid_ts_to_flat_idx[sorted_invalid_indexes[i]] = i
    return [SpaceTimeIndex(FlatIndex(True, invalid_ts_to_flat_idx[orig_idx.flat_idx.idx]), orig_idx.s, orig_idx.t)
            if orig_idx.flat_idx.invalid else orig_idx
            for orig_idx in indexes]

def flatten(l):
    return [item for sublist in l for item in sublist]

@functools.lru_cache(maxsize=None, typed=False)
def dimensions_to_space_time_index(dims, t_idx = (), t_len = (), s_idx = (), s_len = (),
                                   next_idx_valid = 0, invalid = False, first_call = True) -> typing.Tuple[List[SpaceTimeIndex], int]:
    """
    Convert a space-time Type to a flat list of SpaceTimeIndexs with the s and t values along with the flat_idx.
    This is a recursive function. The parameters other than dim are the statuses of the current call.
    The values are needed to compute the flat t, s, and flat_idx of each inner value
    :param dims: The type, it's space and time dimensions
    :param t_idx: The index in each of the parent calls' that are TSeqs
    :param t_len: The lengths of each of the parent calls' TSeqs
    :param s_idx: The index in each of the parent calls' that are SSeqs
    :param S_len: The lengths of each of the parent calls' SSeqs
    :param next_idx_valid: The next flat_idx to use for valids
    :param invalid: Whether this call is in and invalid part of a type. Any invalid parent makes all the children
    invalid
    :return: A list of SpaceTimeIndex
    """
    if type(dims) == ST_SSeq or type(dims) == ST_SSeq_Tuple:
        nested_result = []
        for s in range(dims.n):
            (res, next_idx_valid) = \
                dimensions_to_space_time_index(dims.t, t_idx, t_len,
                                               tuple([s]) + s_idx, tuple([dims.n]) + s_len,
                                               next_idx_valid, invalid, False)
            nested_result += [res]
        result = flatten(nested_result), next_idx_valid
    elif type(dims) == ST_TSeq:
        nested_result = []
        for t in range(dims.n + dims.i):
            (res, next_idx_valid) = \
                dimensions_to_space_time_index(dims.t, tuple([t]) + t_idx, tuple([dims.n + dims.i]) + t_len,
                                               s_idx, s_len, next_idx_valid,
                                               invalid or (t >= dims.n), False)
            nested_result += [res]
        result = flatten(nested_result), next_idx_valid
    else:
        # track how much time each t_idx indicates due to nested index structure
        # drop the last value because each t_idx time is the product of all
        # time dimensions inside of it. No t_idx contains last dimension
        time_per_t_len = list(accumulate([1] + list(t_len), lambda x,y : x*y))[:-1]
        t_idx_with_time_per_len = zip(time_per_t_len, list(t_idx))
        time_per_t_idx = list(map(lambda x: x[0]*x[1], t_idx_with_time_per_len))
        t = reduce(lambda x,y: x+y, [0] + time_per_t_idx)
        # do same computation for space
        time_per_s_len = list(accumulate([1] + list(s_len), lambda x,y : x*y))
        s_idx_with_time_per_len = zip(time_per_s_len, list(s_idx))
        time_per_s_idx = list(map(lambda x: x[0]*x[1], s_idx_with_time_per_len))
        s = reduce(lambda x,y: x+y, [0] + time_per_s_idx)
        if invalid:
            result = [SpaceTimeIndex(FlatIndex(True, (t, s)), s, t)], next_idx_valid
        else:
            next_idx_valid += 1
            result = [SpaceTimeIndex(FlatIndex(False, next_idx_valid - 1), s, t)], next_idx_valid

    if first_call:
        return fix_invalid_indexes(result[0]), result[1]
    else:
        return result





