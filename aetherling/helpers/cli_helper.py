"""
This file imports the necessary stuff to try out aetherling files
"""
from magma import *
from mantle import *
from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import GetCoreIRModule

c = coreir.Context()
cirb = CoreIRBackend(c)

def save_CoreIR_json(module, file):
    nonlocal cirb
    mod = GetCoreIRModule(cirb, module)
    mod.save_to_file(file)
