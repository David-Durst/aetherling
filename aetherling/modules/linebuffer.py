from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import DefineCircuitFromGeneratorWrapper
from ..helpers.nameCleanup import cleanName
from magma.array import ArrayKind

"""
Instance* lbInst = def->addInstance("conv1DLineBuffer", "commonlib.linebuffer", {
                    {"input_type", Const::make(c, lbInType)},
                    {"output_type", Const::make(c, lbOutType)},
                    {"image_type", Const::make(c, lbImgType)},
                    {"has_valid", Const::make(c, true)}
                });
    """

def DefineLinebuffer(cirb: CoreIRBackend, inType: ArrayKind, outType: ArrayKind,
                     imgType: ArrayKind, has_valid=False):
    """
    Implements a linebuffer for image pipelines. inType is an array of how many pixels
    are inputted per clock. outType is an array of how many pixels are emitted per clock.
    imgType is the size of the total image.

    inType and outType are nested arrays if doing linebuffer over a 2d (or higher dimension)
    image. Their nesting should match number of dimensions.

    Args:
        cirb: The CoreIR backend currently be used
        inType: The type of the input every clock
        outType: The type of the output every clock following warmup
        imgType: The type of the complete image
        has_valid: Whether this module should have an output port denoting if output data
        is valid this clock

    Returns:
        A module with the following ports:
        I : In(inType)
        out : Out(outType)
        wen : In(Bit) -- this is a clock enable port. TODO: wrap this module and call it CE

        AND IF SET
        valid : Out(Bit)
    """
    cirInType = cirb.get_type(inType, True)
    cirOutType = cirb.get_type(outType, True)
    cirImgType = cirb.get_type(imgType, True)
    strForValid = "Valid" if has_valid else ""
    name = "linebuffer_in{}_out{}_img{}{}".format(cleanName(str(cirInType)),
                                                  cleanName(str(outType)),
                                                  cleanName(str(imgType)),
                                                  strForValid)
    defToReturn = DefineCircuitFromGeneratorWrapper(cirb, "commonlib", "linebuffer",
                                                         ["mantle", "coreir", "global"],
                                                         name,
                                                         {"input_type": cirInType,
                                                          "output_type": cirOutType,
                                                          "image_type": imgType,
                                                          "has_valid": has_valid})
    return defToReturn


def Linebuffer(cirb: CoreIRBackend, inType: ArrayKind, outType: ArrayKind,
                     imgType: ArrayKind, has_valid=False):
    """
    Implements a linebuffer for image pipelines. inType is an array of how many pixels
    are inputted per clock. outType is an array of how many pixels are emitted per clock.
    imgType is the size of the total image.

    inType and outType are nested arrays if doing linebuffer over a 2d (or higher dimension)
    image. Their nesting should match number of dimensions.

    Args:
        cirb: The CoreIR backend currently be used
        inType: The type of the input every clock
        outType: The type of the output every clock following warmup
        imgType: The type of the complete image
        has_valid: Whether this module should have an output port denoting if output data
        is valid this clock

    Returns:
        A module with the following ports:
        I : In(inType)
        out : Out(outType)
        wen : In(Bit) -- this is a clock enable port. TODO: wrap this module and call it CE

        AND IF SET
        valid : Out(Bit)
    """
    return DefineLinebuffer(cirb, inType, outType, imgType, has_valid)