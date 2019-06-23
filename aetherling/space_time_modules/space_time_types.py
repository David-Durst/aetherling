"""
The classes for constructing types in Aetherling's space-time IR
"""

from magma import Type, Kind, Array, Bit, Tuple

class ST_TSeq():
    n: int
    i: int
    t: Kind

    def __init__(self, n, i, t):
        self.n = n
        self.i = i
        self.t = t

    def length(self):
        return self.n * self.t.length()

    def time(self):
        return (self.n + self.i) * self.t.length()

    def magma_repr(self):
        return self.t.magma_repr()

class ST_SSeq():
    n: int
    t: Kind

    def __init__(self, n, t):
        self.n = n
        self.t = t

    def length(self):
        return self.n * self.t.length()

    def time(self):
        return self.t.time()

    def magma_repr(self):
        return Array[self.n, self.t.magma_repr()]

class ST_SSeq_Tuple():
    n: int
    t: Kind

    def __init__(self, n, t):
        self.n = n
        self.t = t

    def length(self):
        return self.n * self.t.length()

    def time(self):
        return self.t.time()

    def magma_repr(self):
        return Array[self.n, self.t.magma_repr()]

class ST_Atom_Tuple():
    t0: Kind
    t1: Kind

    def __init__(self, t0, t1):
        self.t0 = t0
        self.t1 = t1

    def length(self):
        return self.t0.length() + self.t1.length()

    def time(self):
        return 1

    def magma_repr(self):
        return Tuple(self.t0.magma_repr(), self.t1.magma_repr())

class ST_Int():
    def __init__(self):
        return

    def length(self):
        return 8

    def time(self):
        return 1

    def magma_repr(self):
        return Array[8, Bit]

class ST_Bit():
    def __init__(self):
        return

    def length(self):
        return 1

    def time(self):
        return 1

    def magma_repr(self):
        return Bit

def throughput(t):
    return t.length() / t.time()
