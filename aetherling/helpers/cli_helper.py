"""
This file imports the necessary stuff to try out aetherling files
"""
from magma import *
from mantle import *
from magma.backend.coreir_ import CoreIRBackend
from magma.frontend.coreir_ import GetCoreIRModule

def make_context_and_backend():
    c = coreir.Context()
    return c, CoreIRBackend(c)

def save_CoreIR_json(cirb, module, file):
    mod = GetCoreIRModule(cirb, module)
    mod.save_to_file(file)
