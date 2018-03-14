from magma import *
from magma.backend.coreir_ import CoreIRBackend
from mantle import Register, CounterModM, Mux, Decode
from mantle.common.operator import *
from magma.coreirModuleWrapper import ModuleFromGeneratorWrapper
import math

__all__ = ['DefineUpParallel', 'UpParallel']

@cache_definition
def DefineUpParallel(n, T):
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
        def definition(upParallel):
            for i in range(n):
                wire(upParallel.I, upParallel.O[i])
    return UpParallel

def UpParallel(n, T):
    return DefineUpParallel(n, T)()
# dehydrate = cirb.context.import_generator("aetherlinglib", "dehydrate")(hydratedType = cirb.get_type(Array(3, Array(5, BitIn)), True))
@cache_definition
def DefineUpSequential(cirb: CoreIRBackend, n, T, has_ce=False, has_reset=False):
    """
    Upsample a single T to a stream of T's over n clock cycles.
    Ready is asserted on the clock cycle when a new input is accepted
    Aetherling Type: {1, T} -> {n, T}

    I : In(T)
    O : Out(T)
    Ready : In(Bit)
    """
    class UpSequential(Circuit):
        name = "UpSequential"
        IO = ['I', In(T), 'O', Out(T), 'ready', Out(Bit)] + ClockInterface(has_ce, has_reset)
        @classmethod
        def definition(upSequential):
            #CoreIRBackend.get_type
            #dehydrate = cirb.context.import_generator("aetherlinglib", "dehydrate")\
            #    (hydratedType = cirb.get_type(T))
            #hydrate = cirb.context.import_generator("aetherlinglib", "hydrate")\
            #    (hydratedType = cirb.get_type(T))
            #DeclareCircuit(f"coreir_add{width}", *coreir_io,
            #               coreir_name="aeDehydrate", coreir_lib="aetherlinglib",
            #               coreir_genargs={"hydratedType": },
            #               simulate=simulate_coreir_add)
            dehydrate = ModuleFromGeneratorWrapper(cirb, "aetherlinglib", "dehydrate",
                                           {"hydratedType": cirb.get_type(T)})
            hydrate = ModuleFromGeneratorWrapper(cirb, "aetherlinglib", "hydrate",
                                                   {"hydratedType": cirb.get_type(T)})
            valueStoreReg = Register(T.size, has_ce=True)
            mux = Mux(width=T.size)
            counter = CounterModM(n, math.ceil(math.log(n, 2)))
            eq0 = Decode(0, n)(counter.O)
            wire(upSequential.I, dehydrate.I)
            wire(dehydrate.out, valueStoreReg.I)
            # on first clock cycle, send the input directly out. otherwise, use the register
            wire(eq0, mux.S)
            wire(valueStoreReg.O, mux.I0)
            wire(upSequential.I, mux.I1)
            wire(mux.O, hydrate.I)
            wire(hydrate.O, upSequential.O)
            # only store on the first clock cycle
            wire(eq0, valueStoreReg.CE)
    return UpSequential

def UpSequential(cirb: CoreIRBackend, n, T, has_ce=False, has_reset=False):
    return DefineUpSequential(cirb, n, T, has_ce, has_reset)()

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