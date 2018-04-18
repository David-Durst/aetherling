import math

from mantle import Register, SizedCounterModM, Decode
from mantle.common.operator import *
from mantle.coreir.type_helpers import Term

from magma import *
from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import CircuitInstanceFromGeneratorWrapper
from .hydrate import Dehydrate, Hydrate
from .mapFullyParallelSequential import MapParallel

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
        name = "DownsampleParallel_n{}_T{}".format(str(n), str(T))
        IO = ['I', In(Array(n, T)), 'O', Out(T)]
        @classmethod
        def definition(downsampleParallel):
            one_input_dehydrate = Dehydrate(cirb, T)
            # dehydrate all but the first, that one is passed through
            inputs_dehydrate = MapParallel(cirb, n - 1, one_input_dehydrate)
            term = Term(cirb, one_input_dehydrate.size)
            inputs_term = MapParallel(cirb, n - 1, term)
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
def DefineDownsampleSequential(n, T, has_ce=False, has_reset=False):
    """
    Downsample a stream of T's to a single T over n clock cycles.
    VALID is asserted on the clock cycle when valid output is coming out
    Aetherling Type: {n, T} -> {1, T}

    I : In(T)
    O : Out(T)
    VALID : Out(Bit)
    """
    class DownsampleSequential(Circuit):
        name = "DownsampleSequential_n{}_T{}_hasCE{}_hasReset{}".format(str(n)),\
               str(T) + "_hasCE" + str(has_ce) + str(has_reset)
        IO = ['I', In(T), 'O', Out(T), 'VALID', Out(Bit)] + ClockInterface(has_ce, has_reset)

        @classmethod
        def definition(downsampleSequential):
            counter = SizedCounterModM(n, has_ce=has_ce or has_reset)
            eq0 = Decode(0, counter.O.N)(counter.O)
            wire(downsampleSequential.I, downsampleSequential.O)
            # on first clock cycle, send the input directly out. otherwise, use the register
            wire(eq0, downsampleSequential.VALID)

            # reset counter on clock enable or reset, setup both reset and CE for reg
            if has_ce and has_reset:
                wire(counter.CE, And(2)(downsampleSequential.RESET, downsampleSequential.CE))
            elif has_ce:
                wire(counter.CE, downsampleSequential.CE)
            if has_reset:
                wire(counter.RESET, downsampleSequential.RESET)

    return DownsampleSequential

def DownsampleSequential(n, T, has_ce=False, has_reset=False):
    return DefineDownsampleSequential(n, T, has_ce, has_reset)()
