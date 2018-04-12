# Aetherling

## What is this?
Aetherling is a library for creating data-parallel pipelines in hardware. Aetherling will eventually offer two APIs:
1. Auto-scheduled - The user declares high-level operations like map and reduce. Aetherling optimally maps these to parallel or sequential implementations in hardware compiler transformations that are proven to be optimal.
1. Manually-scheduled  - The user creates pipelines using Aetherlings low-level primitives for a hand-crafted, artisanal data pipeline that is parallelized exactly the desired amount.

## How do I install it?

1. Install the following components in the proscribed order:
    1. [CoreIR](https://github.com/rdaly525/coreir/tree/master) - use the dev branch
        1. This is the IR used to represent the components.
        1. How to install: https://github.com/rdaly525/coreir/blob/master/INSTALL.md
            1. Install from source
            1. After checking the repo out, switch to the dev branch before installing.
            1. Be sure to follow the instructions under the "to build" and "install to /usr or /usr/bin" headings
    1. [Magma](https://github.com/phanrahan/magma) - use the coreir-dev branch
        1. This provides the Python API for creating circuits and basic computer architecture components.
        1. How to install: https://github.com/phanrahan/magma#installation
            1. After checking the repo out, switch to the coreir-dev branch before installing.
    1. [PyCoreIR](https://github.com/leonardt/pycoreir)
        1. This provides the Python API for interfacing with CoreIR.
        1. How to install: https://github.com/leonardt/pycoreir#development-setup
            1. After checking the repo out, switch to the dev branch before installing.
            1. Follow the development installation instructions.
    1. [Mantle](https://github.com/phanrahan/mantle)
        1. This is a library of useful Magma components.
        1. How to install: https://github.com/phanrahan/mantle#setup
            1. After checking the repo out, switch to the coreir-dev branch before installing.
1. Install Aetherling:
```
git clone https://github.com/David-Durst/aetherling/
cd aetherling
pip install -r requirements.txt
pip install -e .
```



