from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import DefineCircuitFromGeneratorWrapper
from ..helpers.nameCleanup import cleanName
from magma.t import Kind

def DefineDehydrate(cirb: CoreIRBackend, T: Kind):
    """
    Convert a nested type to a flat array of bits
    Aetherling Type: {1, T} -> {1, Bit[width(T)]}
    This returns a circuit definition.

    Args:
        cirb: The CoreIR backend currently be used
        T: The type to dehydrate

    Returns:
        A module with the following ports:
        I : In(T)
        out : Out(Array[width(T), Bit])

        The module also has the following data:
        size: width(T)
    """
    cirType = cirb.get_type(T)
    name = "dehydrate_t{}".format(cleanName(str(T)))
    defToReturn = DefineCircuitFromGeneratorWrapper(cirb, "aetherlinglib", "dehydrate", name,
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         {"hydratedType": cirType})
    defToReturn.size = cirType.size
    return defToReturn


def Dehydrate(cirb: CoreIRBackend, T: Kind):
    """
    Convert a nested type to a flat array of bits
    Aetherling Type: {1, T} -> {1, Bit[width(T)]}
    This returns a circuit instance, an instance of a circuit definition

    Args:
        cirb: The CoreIR backend currently be used
        T: The type to dehydrate

    Returns:
        A module with the following ports:
        I : In(T)
        out : Out(Array[width(T), Bit])

        The module also has the following data:
        size: width(T)
    """
    dehydrateDef = DefineDehydrate(cirb, T)
    dehydrateInstance = dehydrateDef()
    dehydrateInstance.size = dehydrateDef.size
    return dehydrateInstance

def DefineHydrate(cirb: CoreIRBackend, T):
    """
    Convert a flat array of bits to a nested type
    Aetherling Type: {1, Bit[width(T)]} -> {1, T}
    This returns a circuit definition.

    Args:
        cirb: The CoreIR backend currently be used
        T: The type to hydrate

    Returns:
        A module with the following ports:
        I : In(Array[width(T), BitIn])
        out : Out(T)
        The module also has the following data:
        size: width(T)
    """
    cirType = cirb.get_type(T)
    name = "hydrate_t{}".format(cleanName(str(T)))
    defToReturn = DefineCircuitFromGeneratorWrapper(cirb, "aetherlinglib", "hydrate", name,
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         {"hydratedType": cirType})
    defToReturn.size = cirType.size
    return defToReturn

def Hydrate(cirb: CoreIRBackend, T):
    """
    Convert a flat array of bits to a nested type
    Aetherling Type: {1, Bit[width(T)]} -> {1, T}
    This returns a circuit instance, an instance of a circuit definition

    Args:
        cirb: The CoreIR backend currently be used
        T: The type to hydrate

    Returns:
        A module with the following ports:
        I : In(Array[width(T), BitIn])
        out : Out(T)
        The module also has the following data:
        size: width(T)
    """
    hydrateDef = DefineHydrate(cirb, T)
    hydrateInstance = hydrateDef()
    hydrateInstance.size = hydrateDef.size
    return hydrateInstance