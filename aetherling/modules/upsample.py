import math

from mantle import Register, SizedCounterModM, Decode
from mantle.common.operator import *

from magma import *
from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import ModuleFromGeneratorWrapper
from .hydrate import Hydrate, Dehydrate

__all__ = ['DefineUpsampleParallel', 'UpsampleParallel', 'DefineUpsampleSequential', 'UpsampleSequential']

@cache_definition
def DefineUpsampleParallel(n, T):
    """
    Upsample a single T to an array of T's in one clock cycle.
    Aetherling Type: {1, T} -> {1, T[n]}

    I : In(T)
    O : Out(Array(n, T))
    """
    class UpParallel(Circuit):
        name = "UpParallel"
        IO = ['I', In(T), 'O', Out(Array(n, T))]
        @classmethod
        def definition(upsampleParallel):
            for i in range(n):
                wire(upsampleParallel.I, upsampleParallel.O[i])
    return UpParallel

def UpsampleParallel(n, T):
    return DefineUpsampleParallel(n, T)()

@cache_definition
def DefineUpsampleSequential(cirb: CoreIRBackend, n, T, has_ce=False, has_reset=False):
    """
    Upsample a single T to a stream of T's over n clock cycles.
    Ready is asserted on the clock cycle when a new input is accepted
    Aetherling Type: {1, T} -> {n, T}

    I : In(T)
    O : Out(T)
    READY : In(Bit)
    """
    class UpSequential(Circuit):
        name = "UpSequential"
        IO = ['I', In(T), 'O', Out(T), 'READY', Out(Bit)] + ClockInterface(has_ce, has_reset)
        @classmethod
        def definition(upsampleSequential):
            dehydrate = Dehydrate(cirb, T)
            hydrate = Hydrate(cirb, T)
            valueStoreReg = Register(dehydrate.size, has_ce=has_ce, has_reset=has_reset)
            mux = Mux(width=dehydrate.size)
            counter = SizedCounterModM(n, has_ce=has_ce or has_reset)
            eq0 = Decode(0, counter.O.N)(counter.O)
            wire(upsampleSequential.I, dehydrate.I)
            wire(dehydrate.out, valueStoreReg.I)
            # on first clock cycle, send the input directly out. otherwise, use the register
            wire(eq0, mux.S)
            wire(valueStoreReg.O, mux.I0)
            wire(upsampleSequential.I, mux.I1)
            wire(mux.O, hydrate.I)
            wire(hydrate.out, upsampleSequential.O)
            wire(eq0, upsampleSequential.READY)

            # reset counter on clock enable or reset, setup both reset and CE for reg
            if has_ce and has_reset:
                wire(counter.CE, And(2)(upsampleSequential.RESET, upsampleSequential.CE))
            if has_ce:
                wire(valueStoreReg.CE, upsampleSequential.CE)
                if not has_reset:
                    wire(counter.CE, upsampleSequential.CE)
            if has_reset:
                wire(valueStoreReg.RESET, upsampleSequential.RESET)
                if not has_ce:
                    wire(counter.RESET, upsampleSequential.RESET)

    return UpSequential

def UpsampleSequential(cirb: CoreIRBackend, n, T, has_ce=False, has_reset=False):
    return DefineUpsampleSequential(cirb, n, T, has_ce, has_reset)()

"""
from coreir.context import *
from magma.backend.coreir_ import CoreIRBackend
from magma.coreirModuleWrapper import ModuleFromGeneratorWrapper
from magma import *
c = Context()
cirb = CoreIRBackend(c)
x = ModuleFromGeneratorWrapper(cirb, "aetherlinglib", "dehydrate", {"hydratedType": cirb.get_type(Array(3, Array(5, BitIn)), True)})
dehydrate = cirb.context.import_generator("aetherlinglib", "dehydrate")(hydratedType = cirb.get_type(Array(3, Array(5, BitIn)), True))
cirb.context.give_coreir_module_definition(dehydrate)

"""