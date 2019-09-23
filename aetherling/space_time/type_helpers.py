from aetherling.space_time.space_time_types import *
from dataclasses import dataclass, replace
from magma import In, Out, Circuit
from typing import Any
import copy

valid_ports = ['valid_up', In(Bit), 'valid_down', Out(Bit)]

@dataclass
class SharedDiffTypes:
    """
    The result of calling get_shared_and_diff_subtypes, which splits two types into their shard and different components
    """
    shared_inner: ST_Type
    shared_outer: ST_Type
    diff_input: ST_Type
    diff_output: ST_Type

@dataclass(frozen=True)
class ST_Tombstone:
    """
    A tombstone node. Present to end ST_Type trees that will be connected with other trees (like a diff with a shread_inner)
    """
    def length(self):
        return 1

    def time(self):
        return 1

    def port_width(self):
        return 1

    def magma_repr(self):
        raise Exception("shouldn't call magma_repr on tombstone.")

def replace_tombstone(outer_type: ST_Type, inner_type: ST_Type) -> ST_Type:
    """
    Replace the ST_Tombstone in outer_type with inner_type
    """
    if outer_type == ST_Tombstone():
        return inner_type
    elif is_nested(outer_type):
        inner_t = replace_tombstone(outer_type.t, inner_type)
        return replace(outer_type, t=inner_t)
    else:
        raise Exception("Can't replace tomsbone for outer_type {} that has no tombstone. "
                        "Inner type was {}.".format(str(outer_type), str(inner_type)))

def remove_tseqs(t: ST_Type) -> ST_Type:
    """
    Get just the sseqs and the non-nested types, removing the tseqs
    """
    if type(t) == ST_SSeq:
        inner_tseqs_removed = remove_tseqs(t.t)
        return replace(t, t=inner_tseqs_removed)
    elif is_nested(t):
        return remove_tseqs(t.t)
    else:
        return t

def num_nested_layers(t: ST_Type) -> int:
    """
    Get the number of TSeqs and SSeqs in t
    """
    if is_nested(t):
        return num_nested_layers(t.t) + 1
    else:
        return 0

def get_shared_and_diff_subtypes(input_type: ST_Type, output_type: ST_Type) -> SharedDiffTypes:
    """
    Given two types that take equal amount of time, have the same length, but have different
    sseq/tseq outer components, split the types into a shared core and different outer components

    :param input_type:
    :param output_type:
    :return: None if no shared base, otherwise SharedDiffTypes
    """
    # this case is for shared inner
    if input_type == output_type:
        return SharedDiffTypes(input_type, ST_Tombstone(), ST_Tombstone(), ST_Tombstone())
    elif is_nested(input_type) and is_nested(output_type):
        inner_shared_diff = get_shared_and_diff_subtypes(input_type.t, output_type.t)

        # replace all inner parts of input and output for comparing this layer
        input_copy = replace(input_type, t=ST_Tombstone())
        output_copy = replace(output_type, t=ST_Tombstone())

        # this case is for shared outer
        if input_copy == output_copy:
            shared_outer = replace(input_copy, t=inner_shared_diff.shared_outer)
            return SharedDiffTypes(inner_shared_diff.shared_inner, shared_outer, inner_shared_diff.diff_input, inner_shared_diff.diff_output)
        # this case is for diff
        else:
            # if already did a shared_outer, then the the shared outer is part of a larger diff
            # take the shared outer and put it on top of both the diff_input and diff_output
            # so return none
            if inner_shared_diff.shared_outer != ST_Tombstone():
                new_diff_input = replace_tombstone(inner_shared_diff.shared_outer, inner_shared_diff.diff_input)
                new_diff_output = replace_tombstone(inner_shared_diff.shared_outer, inner_shared_diff.diff_output)
                inner_shared_diff = replace(inner_shared_diff, diff_input=new_diff_input, diff_output=new_diff_output)
            input_diff = replace(input_copy, t=inner_shared_diff.diff_input)
            output_diff = replace(output_copy, t=inner_shared_diff.diff_output)
            return SharedDiffTypes(inner_shared_diff.shared_inner, ST_Tombstone(), input_diff, output_diff)
    else:
        return None

