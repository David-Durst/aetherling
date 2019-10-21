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

def replace_atom_tuple_with_t0(outer_type: ST_Type) -> ST_Type:
    if is_nested(outer_type):
        inner_t = replace_atom_tuple_with_t0(outer_type.t)
        return replace(outer_type, t=inner_t)
    elif type(outer_type) == ST_Atom_Tuple:
        return outer_type.t0
    else:
        raise Exception("Called replace_atom_tuple_with_t0 on type without atom_tuple")

def remove_tseqs(t: ST_Type) -> ST_Type:
    """
    Get just the sseqs and the non-nested types, removing the tseqs
    """
    if type(t) == ST_SSeq or type(t) == ST_SSeq_Tuple:
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

def num_nested_space_layers(t: ST_Type) -> int:
    """
    Get the number of TSeqs and SSeqs in t
    """
    no_tseqs_t = remove_tseqs(t)
    if is_nested(no_tseqs_t):
        return num_nested_layers(no_tseqs_t.t) + 1
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
    elif is_nested(input_type):
        inner_shared_diff = get_shared_and_diff_subtypes(input_type.t, output_type)
        # replace all inner parts of input so can stack this layer of input
        # on input_diff
        input_copy = replace(input_type, t=ST_Tombstone())
        input_diff = replace(input_copy, t=inner_shared_diff.diff_input)
        return replace(inner_shared_diff, diff_input=input_diff)
    elif is_nested(output_type):
        inner_shared_diff = get_shared_and_diff_subtypes(input_type, output_type.t)
        # replace all inner parts of input so can stack this layer of input
        # on input_diff
        output_copy = replace(output_type, t=ST_Tombstone())
        output_diff = replace(output_copy, t=inner_shared_diff.diff_output)
        return replace(inner_shared_diff, diff_output=output_diff)
    else:
        return None

def strip_tseq_1_0_sseq_1(t: ST_Type) -> ST_Type:
    if is_nested(t):
        # strip SSeq 1 or TSeq 1 0 from t if outer layer of t, then continue on inner layers
        t_no_inner = replace(t, t=ST_Tombstone())
        if t_no_inner == ST_SSeq(1, ST_Tombstone()) or t_no_inner == ST_TSeq(1, 0, ST_Tombstone()):
            return strip_tseq_1_0_sseq_1(t.t)
        else:
            return replace(t, t=strip_tseq_1_0_sseq_1(t.t))
    else:
        return t

def strip_tseq_1_n_sseq_1(t: ST_Type) -> ST_Type:
    if is_nested(t):
        # strip SSeq 1 or TSeq 1 0 from t if outer layer of t, then continue on inner layers
        t_no_inner = replace(t, t=ST_Tombstone())
        if t_no_inner == ST_SSeq(1, ST_Tombstone()) or \
                (type(t_no_inner) == ST_TSeq and t_no_inner.n == 1):
            return strip_tseq_1_0_sseq_1(t.t)
        else:
            return replace(t, t=strip_tseq_1_0_sseq_1(t.t))
    else:
        return t

def flatten(l):
    return [item for sublist in l for item in sublist]

def get_nested_ports(port, nesting_layers, cur_ports = []):
    if nesting_layers > 0:
        for sub_port in port:
            get_nested_ports(sub_port, nesting_layers - 1, cur_ports)
    else:
        cur_ports.append(port)
    return cur_ports

def time_last_valid(t: ST_Type, top=True) -> int:
    if is_nested(t):
        if type(t) == ST_TSeq:
            if top:
                return (t.n-1)*time_last_valid(t.t, False)
            else:
                return (t.n+t.i)*time_last_valid(t.t, False)
        else:
            return time_last_valid(t.t, top)
    else:
        # using 1 in multiplies if not top, else 0 as no later elements
        if top:
            return 0
        else:
            return 1
