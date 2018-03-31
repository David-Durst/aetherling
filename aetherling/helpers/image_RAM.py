from PIL import Image
from bitarray import bitarray
from mantle.coreir.memory import DefineCoreirMem, getRAMAddrWidth
from magma import *
from functools import lru_cache

__all__ = ['RAMInterface', 'ImageRAM', 'LoadImageRAMForSimulation']

class IMGData:
    def __init__(self, img, imgAsBits, pxPerClock):
        self.imgAsBits = imgAsBits
        self.bitsPerPixel = len(img.getbands())*8
        self.bitsPerRow = self.bitsPerPixel*pxPerClock
        self.numRows = len(imgAsBits) / self.bitsPerRow
        assert len(imgAsBits) % self.bitsPerRow == 0, \
            "Can't evenly divide the image into rows of " \
            "bits using the pxPerClock %i" % pxPerClock

# only load all used images once, keep them in RAM for run
@lru_cache(maxsize=None)
def loadImage(imgSrc, pxPerClock):
    img = Image.open(imgSrc)
    imgAsBits = bitarray(endian='little')
    imgAsBits.frombytes(img.tobytes())
    return IMGData(img, imgAsBits, pxPerClock)

def RAMInterface(imgSrc, memoryInput, memoryOutput):
    imgData = loadImage(imgSrc)
    returnedInterface = []
    addrWidth = getRAMAddrWidth(imgData.numRows)
    if memoryInput:
        returnedInterface += ["Input_WADDR", In(Bits(addrWidth)),
            "Input_WDATA", In(Bits(imgData.bitsPerRow)), "Input_WEN", In(Bit)]
    if memoryOutput:
        returnedInterface += ["Output_RADDR", Out(Bits(addrWidth)),
            "Output_RDATA", Out(Bits(imgData.bitsPerRow))]
    return returnedInterface

def ImageRAM(circuit, nextNode, imgSrc, pxPerClock):
    imgData = loadImage(imgSrc)
    imgRAM = DefineCoreirMem(imgData.numRows, imgData.bitsPerRow)()
    wire(circuit.WADDR, imgRAM.WADDR)
    wire(circuit.WDATA, imgRAM.WDATA)
    wire(imgRAM.RDATA, )
    wire(circuit.RE, imgRAM.RE)
    wire(circuit.WE, imgRAM.WE)
    return imgRAM

def LoadImageRAMForSimulation(sim, scope, imgSrc):
    imgData = loadImage(imgSrc)
    for i in range(imgData.numRows):
        sim.set_value(sim.W, , scope)
        sim.evaluate()
        sim.advance_cycle()