from magma import *
from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import CircuitInstanceFromGeneratorWrapper, GetCoreIRModule
from ..helpers.nameCleanup import cleanName

def Flatten(cirb: CoreIRBackend, inputType: Kind, singleElementOutputType: Kind) -> Circuit:
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
    O : Out(Array(singleElementOutputType, k*...*s))
    """
    cirInputType = cirb.get_type(inputType, True)
    cirSingleEleementOutputType = cirb.get_type(singleElementOutputType, True)
    name = "dehydrate_tin{}_tout".format(cleanName(str(cirInputType)), cleanName(str(singleElementOutputType)))
    moduleToReturn = CircuitInstanceFromGeneratorWrapper(cirb, "aetherlinglib", "flatten",
                                                         ["commonlib", "mantle", "coreir", "global"],
                                                         name,
                                                         {"inputType": cirInputType,
                                                          "singleElementOutputType": cirSingleEleementOutputType})
    return moduleToReturn
