from magma import *
from magma.frontend.coreir_ import CircuitInstanceFromGeneratorWrapper, GetCoreIRModule, GetCoreIRBackend
from ..helpers.nameCleanup import cleanName

@cache_definition
def Flatten(inputType: Kind, singleElementOutputType: Kind) -> Circuit:
    """
    Flatten a nested list to a single list. The nested list can be flattened across multiple dimensions
    with a single flatten node. The output list can be nested, but if so must be flatter than the input list.
    Aetherling Type: T[k]...[s] -> T[k*...*s] (... means potentially many lists/lengths)

    :param cirb: The CoreIR backend currently be used
    :param inputType: The nested lists input type
    :param singleElementOutputType: The element type T that will be emitted. It can be a list itself, but it must be
     an element (at some level of nesting) in inputType.
    :return: A list with elements of type singleElementOutputType that is of the appropriate length
    for the flatteneing.
    I : In(inputType)
    O : Out(Array[singleElementOutputType, k*...*s)])
    """
    cirb = GetCoreIRBackend()
    cirInputType = cirb.get_type(inputType)
    cirSingleEleementOutputType = cirb.get_type(singleElementOutputType)
    name = "dehydrate_tin{}_tout".format(cleanName(str(cirInputType)), cleanName(str(singleElementOutputType)))
    moduleToReturn = CircuitInstanceFromGeneratorWrapper(cirb, "aetherlinglib", "flatten", name,
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         {"inputType": cirInputType,
                                                          "singleElementOutputType": cirSingleEleementOutputType})
    return moduleToReturn
