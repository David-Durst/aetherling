from magma.frontend.coreir_ import DefineCircuitFromGeneratorWrapper, GetCoreIRBackend
from ..helpers.nameCleanup import cleanName
from magma.t import Kind
from magma import In, cache_definition

@cache_definition
def DefineDehydrate(T: Kind):
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
    cirb = GetCoreIRBackend()
    cirType = cirb.get_type(In(T))
    name = "dehydrate_t{}".format(cleanName(str(T)))
    defToReturn = DefineCircuitFromGeneratorWrapper(cirb, "aetherlinglib", "dehydrate", name,
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         {"hydratedType": cirType}, print_error=True)
    defToReturn.size = cirType.size
    #print(f"Current Dehydrate input type {T} and output IO {defToReturn.IO}")
    return defToReturn


def Dehydrate(T: Kind):
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
    dehydrateDef = DefineDehydrate(T)
    dehydrateInstance = dehydrateDef()
    dehydrateInstance.size = dehydrateDef.size
    return dehydrateInstance

@cache_definition
def DefineHydrate(T):
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
    cirb = GetCoreIRBackend()
    cirType = cirb.get_type(In(T))
    name = "hydrate_t{}".format(cleanName(str(T)))
    defToReturn = DefineCircuitFromGeneratorWrapper(cirb, "aetherlinglib", "hydrate", name,
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         {"hydratedType": cirType})
    defToReturn.size = cirType.size
    return defToReturn

def Hydrate(T):
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
    hydrateDef = DefineHydrate(T)
    hydrateInstance = hydrateDef()
    hydrateInstance.size = hydrateDef.size
    return hydrateInstance