from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import ModuleFromGeneratorWrapper

def Dehydrate(cirb: CoreIRBackend, T):
    """
    Convert a nested type to a flat array of bits
    Aetherling Type: {1, T} -> {1, Bit[width(T)]}

    I : In(T)
    O : Out(Array(width(T), Bit))
    size: width(T)
    """
    cirType = cirb.get_type(T, True)
    moduleToReturn = ModuleFromGeneratorWrapper(cirb, "aetherlinglib", "dehydrate",
                                                ["commonlib", "mantle", "coreir", "global"],
                                                {"hydratedType": cirType})
    moduleToReturn.size = cirType.size
    return moduleToReturn

def Hydrate(cirb: CoreIRBackend, T):
    """
    Convert a flat array of bits to a nested type
    Aetherling Type: {1, Bit[width(T)]} -> {1, T}

    I : In(Array(width(T), BitIn))
    O : In(T)
    size: width(T)
    """
    cirType = cirb.get_type(T, True)
    moduleToReturn = ModuleFromGeneratorWrapper(cirb, "aetherlinglib", "hydrate",
                                                ["commonlib", "mantle", "coreir", "global"],
                                                {"hydratedType": cirType})
    moduleToReturn.size = cirType.size
    return moduleToReturn