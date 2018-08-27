from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import DefineCircuitFromGeneratorWrapper
from ..helpers.nameCleanup import cleanName
from magma import Circuit, Array, ArrayKind, Kind, In, Out, Bit, wire
from mantle.coreir.type_helpers import Term

"""
import coreir
from magma.backend.coreir_ import CoreIRBackend
from aetherling.modules.linebuffer import *
from magma import *

c = coreir.Context()
cirb = CoreIRBackend(c)
lb = Linebuffer1DPartitioned(cirb, 1, 3, Array(3, Bit), Array(30, Array(3, Bit)))
"""
def DefineLinebuffer(cirb: CoreIRBackend, inType: ArrayKind, outType: ArrayKind,
                     imgType: ArrayKind, has_valid=False):
    """
    Implements a linebuffer for image pipelines. inType is an array of how many pixels
    are inputted per clock. outType is an array of how many pixels are emitted per clock.
    imgType is the size of the total image.

    inType and outType are nested arrays of elementType if doing linebuffer over a 2d (or higher dimension)
    image. Their nesting should match number of dimensions.

    inType, outType, and imgType must be arrays of elementType

    Args:
        cirb: The CoreIR backend currently be used
        inType: The type of the input every clock
        outType: The type of the output every clock following warmup
        imgType: The type of the complete image
        elementType: The elements of the image, typically this is a pixel
        has_valid: Whether this module should have an output port denoting if output data
        is valid this clock

    Returns:
        A module with the following ports:
        I : In(inType)
        out : Out(outType)
        wen : In(Bit) -- this is a clock enable port. TODO: wrap this module and call it CE

        AND IF VALID SET
        valid : Out(Bit)
        valid_chain : Out(Bit) (this is an internal property that is being
        exposed on linebuffer's external interface. always send it to a term)
    """
    # Reason for weird False/True settings in get_type
    # get_type does some funky things, False means not input, and since
    # looking from inside module, output port is an input as it receives input
    # But, linebuffer wants these ports from perspective of outside,
    # so need inverse, inputs are BitIns and outputs are Bits
    cirInType = cirb.get_type(inType, False)
    cirOutType = cirb.get_type(outType, True)
    cirImgType = cirb.get_type(imgType, False)
    strForValid = "_Valid" if has_valid else ""
    name = "linebuffer_in{}_out{}_img{}{}".format(cleanName(str(inType)),
                                                  cleanName(str(outType)),
                                                  cleanName(str(imgType)),
                                                  strForValid)
    # this is the linebuffer with the outputs as a flat array, unsplit
    defToReturn = DefineCircuitFromGeneratorWrapper(cirb, "commonlib", "linebuffer", "unioned_" + name,
                                                         ["mantle", "coreir", "global"],
                                                         {"input_type": cirInType,
                                                          "output_type": cirOutType,
                                                          "image_type": cirImgType,
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
    return DefineLinebuffer(cirb, inType, outType, imgType, has_valid)()

# Note: This removes valid_chain for convenience
def DefineLinebuffer1DPartitioned(cirb: CoreIRBackend, pxPerClock: int, stencilWidth: int,
                                  elementType: ArrayKind, imgType: ArrayKind, has_valid = False):
    class _1DLinebuffer(Circuit):
        assert elementType == imgType.T, "For 1D linebuffer, image must be a 1D array of elements"
        strForValid = "_Valid" if has_valid else ""
        cirElementType = cirb.get_type(elementType, False)
        cirImgType = cirb.get_type(imgType, False)

        name = "linebuffer1d_p{}_w{}_img{}_elm{}".format(pxPerClock, stencilWidth, cleanName(str(imgType)),
                                                         cleanName(str(elementType)), strForValid)
        IO = ['I', In(Array(pxPerClock, elementType)), 'O', Out(Array(pxPerClock, Array(stencilWidth, elementType))), "CE", In(Bit)] + \
             (['valid', Bit] if has_valid else [])

        @classmethod
        def definition(cls):
            lb = Linebuffer(cirb, Array(pxPerClock, elementType), Array(stencilWidth + pxPerClock - 1, elementType), imgType)
            overlapPartition = DefineCircuitFromGeneratorWrapper(cirb, "aetherlinglib", "overlapPartition",
                                                             "overlapPartition_" + cls.name,
                                                             ["commonlib", "mantle", "coreir", "global"],
                                                             {"elementType": cls.cirElementType,
                                                              "numOverlapped": pxPerClock,
                                                              "arrayLen": stencilWidth})()

            wire(cls.I, lb.I)
            wire(lb.out, overlapPartition.I)
            wire(overlapPartition.out, cls.O)
            wire(lb.wen, cls.CE)

            if (has_valid):
                cls.wire(lb.valid, cls.valid)
                validChainTerm = Term(cirb, 1)
                wire(lb.valid_chain, validChainTerm.I[0])

    return _1DLinebuffer

def Linebuffer1DPartitioned(cirb: CoreIRBackend, pxPerClock: int, stencilWidth: int, elementType: Kind,
                            imgType: ArrayKind, has_valid = False):
    return DefineLinebuffer1DPartitioned(cirb, pxPerClock, stencilWidth, elementType, imgType, has_valid)()