from aetherling.space_time_modules.space_time_types import *
from itertools import accumulate, groupby
from functools import reduce
from dataclasses import dataclass

@dataclass
class SpaceTimeIndex:
    """
    Class for traccking the flat_idx and space and time indexes of a values
    in a space-time object
    """
    flat_idx: int
    s: int
    t: int

next_idx = 0
def get_output_address_at_input(t:int, s:int, input_type, output_type):
    """
    Given non-nested space-time coordinates in input, get the non-nested
    space-time coordinates for the output
    :param s: non-nested space coordinate in input
    :param t: non-nest time coordinate in output
    :param input_type: input nested Space-Time type
    :param output_type:  output nested Space-Time type
    :return: SpaceTimeIndex non-nested coordinates
    """
    global next_idx
    non_nested_input_ts_vals = dimensions_to_flat_idx(input_type)
    value = non_nested_input_ts_vals[t][s]
    next_idx = 0
    output_ts_value_triples = dimensions_to_flat_idx_helper(output_type)
    return list(filter(lambda x: x.flat_idx == value, output_ts_value_triples))[0]

def get_input_address_at_output(t:int, s:int, input_type, output_type):
    """
    Given non-nested space-time coordinates in output, get the non-nested
    space-time coordinates for the inpput
    :param s: non-nested space coordinate in input
    :param t: non-nest time coordinate in output
    :param input_type: input nested Space-Time type
    :param output_type:  output nested Space-Time type
    :return: SpaceTimeIndex with non-nested coordinates
    """
    global next_idx
    non_nested_output_ts_vals = dimensions_to_flat_idx(output_type)
    value = non_nested_output_ts_vals[t][s]
    next_idx = 0
    input_ts_value_triples = dimensions_to_flat_idx_helper(input_type)
    return list(filter(lambda x: x.flat_idx == value, input_ts_value_triples))[0]

def dimensions_to_flat_idx(dims):
    """
    Convert a nested space-time type into a 2d space-time representation.
    This function tells me when each value should be read and written
    Each outer dimension of the output array is a clock cycle.
    Each inner dimension of the output array is a vector lane.

    :param dims: the space-time type to convert
    """
    global next_idx
    next_idx = 0
    flat_elems = dimensions_to_flat_idx_helper(dims)
    # need to sort before groupby to get actual, sql like groupby
    flattened_sorted = sorted(flat_elems, key=lambda x: x.t)
    flattened_grouped_ts = [list(group) for _, group in groupby(flattened_sorted, lambda x: x.t)]
    # sort by s within each group now
    flattened_ts = list(map(lambda g : sorted(g, key=lambda x: x.s), flattened_grouped_ts))
    return list(map(lambda l : list(map(lambda x : x.flat_idx, l)), flattened_ts))


flatten = lambda l: [item for sublist in l for item in sublist]

def dimensions_to_flat_idx_helper(dims, t_idx = [], t_len = [], s_idx = [], s_len = []):
    global next_idx
    if type(dims) == ST_SSeq or type(dims) == ST_SSeq_Tuple:
        nested_result = [dimensions_to_flat_idx_helper(dims.t, t_idx, t_len, [s] + s_idx, [dims.n] + s_len)
             for s in range(dims.n)]
        return flatten(nested_result)
    elif type(dims) == ST_TSeq:
        nested_result = [dimensions_to_flat_idx_helper(dims.t, [t] + t_idx, [dims.n] + t_len, s_idx, s_len)
             for t in range(dims.n)]
        return flatten(nested_result)
    else:
        # track how much time each t_idx indicates due to nested index structure
        # drop the last value because each t_idx time is the product of all
        # time dimensions inside of it. No t_idx contains last dimension
        time_per_t_len = list(accumulate([1] + t_len, lambda x,y : x*y))[:-1]
        t_idx_with_time_per_len = zip(time_per_t_len, t_idx)
        time_per_t_idx = list(map(lambda x: x[0]*x[1], t_idx_with_time_per_len))
        t = reduce(lambda x,y: x+y, [0] + time_per_t_idx)
        # do same computation for space
        time_per_s_len = list(accumulate([1] + s_len, lambda x,y : x*y))
        s_idx_with_time_per_len = zip(time_per_s_len, s_idx)
        time_per_s_idx = list(map(lambda x: x[0]*x[1], s_idx_with_time_per_len))
        s = reduce(lambda x,y: x+y, [0] + time_per_s_idx)
        next_idx += 1
        return [SpaceTimeIndex(next_idx - 1, s, t)]





