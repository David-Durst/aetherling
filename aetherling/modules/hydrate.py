from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import CircuitInstanceFromGeneratorWrapper
from ..helpers.nameCleanup import cleanName
from magma.t import Kind

__all__ = ["Dehydrate", "Hydrate"]

def Dehydrate(cirb: CoreIRBackend, T: Kind):
    """
    Convert a nested type to a flat array of bits
    Aetherling Type: {1, T} -> {1, Bit[width(T)]}

    Args:
        cirb: The CoreIR backend currently be used
        T: The type to dehydrate

    Returns:
        A module with the following ports:
        I : In(T)
        out : Out(Array(width(T), Bit))

        The module also has the following data:
        size: width(T)
    """
    cirType = cirb.get_type(T, True)
    name = "dehydrate_t{}".format(cleanName(str(T)))
    moduleToReturn = CircuitInstanceFromGeneratorWrapper(cirb, "aetherlinglib", "dehydrate",
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         name,
                                                         {"hydratedType": cirType})
    moduleToReturn.size = cirType.size
    return moduleToReturn

def Hydrate(cirb: CoreIRBackend, T):
    """
    Convert a flat array of bits to a nested type
    Aetherling Type: {1, Bit[width(T)]} -> {1, T}

    Args:
        cirb: The CoreIR backend currently be used
        T: The type to hydrate

    Returns:
        A module with the following ports:
        I : In(Array(width(T), BitIn))
        out : Out(T)
        The module also has the following data:
        size: width(T)
    """
    cirType = cirb.get_type(T, True)
    name = "hydrate_t{}".format(cleanName(str(T)))
    moduleToReturn = CircuitInstanceFromGeneratorWrapper(cirb, "aetherlinglib", "hydrate",
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         name,
                                                         {"hydratedType": cirType})
    moduleToReturn.size = cirType.size
    return moduleToReturn