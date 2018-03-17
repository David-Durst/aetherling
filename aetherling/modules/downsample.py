import math

from mantle import Register, CounterModM, Decode
from mantle.common.operator import *
from mantle.coreir.type_helpers import Term

from magma import *
from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import ModuleFromGeneratorWrapper, GetCoreIRModule
from .hydrate import Dehydrate
from .map import MapParallel

__all__ = ['DefineDownsampleParallel', 'DownsampleParallel',
           'DefineDownsampleSequential', 'DownsampleSequential']

@cache_definition
def DefineDownsampleParallel(cirb: CoreIRBackend, n, T):
    """
    Downsample an array of T's to a single T in one clock cycle.
    Aetherling Type: {1, T[n]} -> {1, T}

    I : In(Array(n, T))
    O : Out(T)
    """
    class DownsampleParallel(Circuit):
        name = "DownsampleParallel"
        IO = ['I', In(Array(n, T)), 'O', Out(T)]
        @classmethod
        def definition(downsampleParallel):
            one_input_dehydrate = Dehydrate(cirb, T)
            # dehydrate all but the first, that one is passed through
            inputs_dehydrate = MapParallel(cirb, n - 1,
                                           GetCoreIRModule(cirb, one_input_dehydrate))
            term = Term(cirb, one_input_dehydrate.size)
            inputs_term = MapParallel(cirb, n - 1, GetCoreIRModule(cirb, term))
            wire(downsampleParallel.I[0], downsampleParallel.O)
            wire(downsampleParallel.I[1:], inputs_dehydrate.I)
            wire(inputs_dehydrate.out, inputs_term.I)
            # have to do this wiring so excess modules that were passed as input to map
            # are ignored
            #wire(one_input_dehydrate.O, term.I)
    return DownsampleParallel

def DownsampleParallel(cirb: CoreIRBackend, n, T):
    return DefineDownsampleParallel(cirb, n, T)()

@cache_definition
def DefineDownsampleSequential(cirb: CoreIRBackend, n, T, has_ce=False, has_reset=False):
    """
    Upsample a single T to a stream of T's over n clock cycles.
    Ready is asserted on the clock cycle when a new input is accepted
    Aetherling Type: {1, T} -> {n, T}

    I : In(T)
    O : Out(T)
    Ready : In(Bit)
    """
    class DownsampleSequential(Circuit):
        name = "DownsampleSequential"
        IO = ['I', In(T), 'O', Out(T), 'READY', Out(Bit)] + ClockInterface(has_ce, has_reset)
        @classmethod
        def definition(upSequential):
            cirType = cirb.get_type(T, True)
            dehydrate = ModuleFromGeneratorWrapper(cirb, "aetherlinglib", "dehydrate",
                                                   ["commonlib", "mantle", "coreir", "global"],
                                                   {"hydratedType": cirType})
            hydrate = ModuleFromGeneratorWrapper(cirb, "aetherlinglib", "hydrate",
                                                 ["commonlib", "mantle", "coreir", "global"],
                                                 {"hydratedType": cirType})
            valueStoreReg = Register(cirType.size, has_ce=has_ce, has_reset=has_reset)
            mux = Mux(width=cirType.size)
            counter = CounterModM(n, math.ceil(math.log(n, 2)) + 1)
            eq0 = Decode(0, n)(counter.O)
            wire(upSequential.I, dehydrate.I)
            wire(dehydrate.out, valueStoreReg.I)
            # on first clock cycle, send the input directly out. otherwise, use the register
            wire(eq0, mux.S)
            wire(valueStoreReg.O, mux.I0)
            wire(upSequential.I, mux.I1)
            wire(mux.O, hydrate.I)
            wire(hydrate.out, upSequential.O)
            wire(eq0, upSequential.READY)

            # reset counter on clock enable or reset, setup both reset and CE for reg
            if has_ce and has_reset:
                wire(counter.CE, And(2)(upSequential.RESET, upSequential.CE))
            if has_ce:
                wire(valueStoreReg.CE, upSequential.CE)
                if not has_reset:
                    wire(counter.CE, upSequential.CE)
            if has_reset:
                wire(valueStoreReg.RESET, upSequential.RESET)
                if not has_ce:
                    wire(counter.RESET, upSequential.RESET)

    return DownsampleSequential

def DownsampleSequential(cirb: CoreIRBackend, n, T, has_ce=False, has_reset=False):
    return DefineDownsampleSequential(cirb, n, T, has_ce, has_reset)()

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