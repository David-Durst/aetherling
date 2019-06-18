"""
The classes for constructing types in Aetherling's space-time IR
"""

from dataclasses import dataclass
from magma import Type, Kind, Array, Bit, Tuple

@dataclass
class ST_TSeq(Type, metaclass=Kind):
    n: int
    i: int
    t: Kind

    def length(self):
        return self.n * self.t.length()

    def time(self):
        return (self.n + self.i) * self.t.length()

    def magma_repr(self):
        return self.t.magma_repr()

@dataclass
class ST_SSeq(Type, metaclass=Kind):
    n: int
    t: Kind

    def length(self):
        return self.n * self.t.length()

    def time(self):
        return self.t.time()

    def magma_repr(self):
        return Array[self.n, self.t.magma_repr()]

@dataclass
class ST_Seq_Tuple(Type, metaclass=Kind):
    t: Kind
    n: int

    def length(self):
        return self.n * self.t.length()

    def time(self):
        return self.t.time()

    def magma_repr(self):
        return Array[self.n, self.t.magma_repr()]

@dataclass
class ST_Atom_Tuple(Type, metaclass=Kind):
    t0: Kind
    t1: Kind

    def length(self):
        return self.t0.length() + self.t1.length()

    def time(self):
        return 1

    def magma_repr(self):
        Tuple(self.t0.magma_repr(), self.t1.magma_repr())

@dataclass
class ST_Int(Type, metaclass=Kind):
    def length(self):
        return 8

    def time(self):
        return 1

    def magma_repr(self):
        return Array[8, Bit]

@dataclass
class ST_Bit(Type, metaclass=Kind):
    def length(self):
        return 1

    def time(self):
        return 1

    def magma_repr(self):
        return Bit

def throughput(t):
    return t.length() / t.time()
