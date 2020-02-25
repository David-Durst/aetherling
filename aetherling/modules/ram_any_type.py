from aetherling.helpers.nameCleanup import cleanName
from magma import *
from magma.frontend.coreir_ import GetCoreIRBackend
from aetherling.modules.hydrate import Dehydrate, Hydrate
from mantle.coreir.memory import DefineRAM, getRAMAddrWidth


__all__ = ['DefineRAMAnyType', 'RAMAnyType']

@cache_definition
def DefineRAMAnyType(t: Kind, n: int, read_latency = 0):
    """
    Generate a RAM that handles n of any type.

    RADDR : In(Array[log_2(n), Bit)], RDATA : Out(t), WADDR : In(Array(log_2(n), Bit)), WDATA : In(t), WE: In(Bit)
    """

    class _RAM(Circuit):
        name = 'RAM_{}t_{}n'.format(cleanName(str(t)), n)
        addr_width = getRAMAddrWidth(n)
        IO = ['RADDR', In(Bits[addr_width]),
              'RDATA', Out(t),
              'WADDR', In(Bits[addr_width]),
              'WDATA', In(t),
              'WE', In(Bit)
             ] + ClockInterface()
        @classmethod
        def definition(cls):
            type_size_in_bits = GetCoreIRBackend().get_type(t).size
            if read_latency == 1:
                ram = DefineCoreirMem2(n, type_size_in_bits)()
                type_to_bits = Dehydrate(t)
                wire(cls.WDATA, type_to_bits.I)
                wire(type_to_bits.out, ram.wdata)

                bits_to_type = Hydrate(t)
                wire(ram.rdata, bits_to_type.I)
                wire(bits_to_type.out, cls.RDATA)

                wire(cls.RADDR, ram.raddr)
                wire(ram.waddr, cls.WADDR)

                wire(cls.WE, ram.wen)
            else:
                ram = DefineRAM(n, type_size_in_bits, read_latency=read_latency)()

                type_to_bits = Dehydrate(t)
                wire(cls.WDATA, type_to_bits.I)
                wire(type_to_bits.out, ram.WDATA)

                bits_to_type = Hydrate(t)
                wire(ram.RDATA, bits_to_type.I)
                wire(bits_to_type.out, cls.RDATA)

                wire(cls.RADDR, ram.RADDR)
                wire(ram.WADDR, cls.WADDR)

                wire(cls.WE, ram.WE)

    return _RAM

def RAMAnyType(t: Kind, n: int):
    return DefineRAMAnyType(t, n)()

def DefineCoreirMem2(depth, width):
    name = "coreir_mem2_{}x{}".format(depth,width)
    addr_width = getRAMAddrWidth(depth)
    IO = ["raddr", In(Bits[ addr_width ]),
          "rdata", Out(Bits[ width ]),
          "waddr", In(Bits[ addr_width ]),
          "wdata", In(Bits[ width ]),
          "clk", In(Clock),
          "wen", In(Bit) ]
    return DeclareCircuit(name, *IO, verilog_name="syncram",
                          coreir_name="SyncRam", coreir_lib="memory",
                          coreir_genargs={"width": width, "depth": depth})
    # coreir_configargs={"init": "0"})

