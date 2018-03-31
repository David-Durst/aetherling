from PIL import Image
from bitarray import bitarray
from mantle.coreir.memory import CoreirMem, getRAMAddrWidth
from magma import *
from mantle import SizedCounterModM
from functools import lru_cache
import math

BITS_PER_PIXEL_BAND = 8

class IMGData:
    def __init__(self, img, imgAsBits, pxPerClock):
        self.imgAsBits = imgAsBits
        self.bitsPerPixel = len(img.getbands())*BITS_PER_PIXEL_BAND
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
    if memoryInput:
        returnedInterface += ["input_wdata", In(Bits(imgData.bitsPerRow)),
                              "input_wen", In(Bit), "input_ren", In(Bit)]
    if memoryOutput:
        returnedInterface += ["output_rdata", Out(Bits(imgData.bitsPerRow)),
                              "output_ren", Out(Bit)]
    return returnedInterface

def InputImageRAM(circuit, nextNodeInput, imgSrc):
    imgData = loadImage(imgSrc)
    imgRAM = CoreirMem(imgData.numRows, imgData.bitsPerRow)

    # this counter ensures writing to correct address always
    writeCounter = SizedCounterModM(imgData.numRows, has_ce=True)
    # this counter ensures reading from the right address
    readCounter = SizedCounterModM(imgData.numRows, has_ce=True)

    wire(writeCounter.O, imgRAM.waddr)
    wire(circuit.input_wdata, imgRAM.wdata)
    wire(readCounter.O, imgRAM.raddr)
    wire(imgRAM.rdata, nextNodeInput)
    wire(circuit.input_ren, readCounter.CE)
    wire(circuit.input_wen, imgRAM.wen)
    wire(circuit.input_wen, writeCounter.CE)
    return imgRAM


def OutputImageRAM(circuit, prevNodeOutput, writeValidSignal, imgSrc):
    imgData = loadImage(imgSrc)
    imgRAM = CoreirMem(imgData.numRows, imgData.bitsPerRow)

    # this counter ensures writing to correct address always
    writeCounter = SizedCounterModM(imgData.numRows, has_ce=True)
    # this counter ensures reading from the right address
    readCounter = SizedCounterModM(imgData.numRows, has_ce=True)

    wire(writeCounter.O, imgRAM.waddr)
    wire(prevNodeOutput, imgRAM.wdata)
    wire(readCounter.O, imgRAM.raddr)
    wire(imgRAM.rdata, circuit.output_rdata)
    wire(circuit.output_ren, readCounter.CE)
    wire(writeValidSignal, imgRAM.wen)
    wire(writeValidSignal, writeCounter.CE)
    return imgRAM

def LoadImageRAMForSimulation(sim, testcircuit, scope, imgSrc):
    imgData = loadImage(imgSrc)
    sim.set_value(testcircuit.input_wen, bits(True), scope)
    sim.set_value(testcircuit.input_ren, bits(False), scope)
    for i in range(imgData.numRows):
        bitsStartIndex = i*imgData.bitsPerRow
        bitsEndIndex = (i+1)*imgData.bitsPerRow-1
        sim.set_value(testcircuit.input_wdata,
                      imgData.imgAsBits[bitsStartIndex:bitsEndIndex], scope)
        sim.evaluate()
        sim.advance_cycle()
    sim.set_value(testcircuit.input_wen, bits(False), scope)
    sim.set_value(testcircuit.input_ren, bits(True), scope)

def DumpImageRAMForSimulation(sim, testcircuit, scope, imgSrc):
    imgData = loadImage(imgSrc)
    sim.set_value(testcircuit.output_ren, bits(True), scope)
    sim.evaluate()
    sim.advance_cycle()
    for i in range(imgData.numRows):
        bitsVals = sim.get_value(testcircuit.input_wdata, scope)
        assert False, "NEED TO LOOK AT bitsVals and figure out how to convert to bits"
        sim.evaluate()
        sim.advance_cycle()
