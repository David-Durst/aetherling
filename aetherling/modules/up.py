from magma import *
from magma.backend.coreir_ import CoreIRBackend
#from mantle import Register
from coreir import context

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
        IO = ['I', In(T), 'O', Out(Array(n, T))]
        @classmethod
        def definition(upParallel):
            for i in range(n):
                wire(upParallel.I, upParallel.O[i])
    return UpParallel

def UpParallel(n, T):
    return DefineUpParallel(n, T)

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
        IO = ['I', In(T), 'O', Out(T), 'ready', Out(Bit)] + ClockInterface(has_ce, has_reset)
        @classmethod
        def definition(upSequential):
            CoreIRBackend.get_type
            dehydrate = cirb.context.import_generator("aetherlinglib", "dehydrate")\
                (hydratedType = cirb.get_type(T))
            hydrate = cirb.context.import_generator("aetherlinglib", "hydrate")\
                (hydratedType = cirb.get_type(T))
            #DeclareCircuit(f"coreir_add{width}", *coreir_io,
            #               coreir_name="aeDehydrate", coreir_lib="aetherlinglib",
            #               coreir_genargs={"hydratedType": },
            #               simulate=simulate_coreir_add)

            #outputReg = Register(T.width)
            wire(upSequential.I, upSequential.O)
    return UpSequential

def UpSequential(n, T, has_ce=False, has_reset=False):
    return DefineUpParallel(n, T, has_ce, has_reset)