"""
This file handles of the magma idiosyncrasies.
"""
from magma.interface import InterfaceKind

def getInputPorts(interface_kind: InterfaceKind) -> list:
    """
    Given an InterfaceKind, get all the input ports.
    NOTE: USE IO not Interface property for getting the interface from outside of a module definition or declaration
    :param interface_kind: The interface kind to get the ports from
    :return: A list of lists where the first entries are names of out ports, second entries are the types.
    """
    return [[name, interface_kind.ports[name]] for name in interface_kind.ports if interface_kind.ports[name].isinput()]

def getOutputPorts(interface_kind: InterfaceKind) -> list:
    """
    Given an InterfaceKind, get all the output ports
    NOTE: USE IO not Interface property for getting the interface from outside of a module definition or declaration
    :param interface_kind: The interface kind to get the ports from
    :return: A list of lists where the first entries are names of out ports, second entries are the types.
    """
    return [[name, interface_kind.ports[name]] for name in interface_kind.ports if interface_kind.ports[name].isoutput()]


