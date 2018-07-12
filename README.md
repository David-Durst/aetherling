# Aetherling
[![Build Status](https://travis-ci.com/David-Durst/aetherling.svg?branch=master)](https://travis-ci.com/David-Durst/aetherling)

## Overview
Aetherling is a library for creating data-parallel pipelines in hardware. Aetherling will eventually offer two APIs:
1. Auto-scheduled - The user declares high-level operations like map and reduce. Aetherling optimally maps these to parallel or sequential implementations in hardware compiler transformations that are proven to be optimal.
1. Manually-scheduled  - The user creates pipelines using Aetherlings low-level primitives for a hand-crafted, artisanal data pipeline that is parallelized exactly the desired amount.

## Documentation
1. Read the [documentation of the stack Aetherling is built on top of](https://github.com/phanrahan/magma/blob/docs/doc/overview.md)
1. The following document provides a description of how much space on hardware and how much time in clock cycles each operation takes: https://github.com/David-Durst/aetherling/blob/master/applied/spacetime.txt
1. aetherling/modules contains the currently implemented modules
1. tests contains a list of unit tests that show how to create Aetherling pipelines.
    1. Run the tests with the following command:
    ```
    pytest -s tests
    ```

## Installation
1. Install the following components in the proscribed order:
    1. [CoreIR](https://github.com/rdaly525/coreir/tree/master)
        1. This is the IR used to represent the components.
        1. How to install: https://github.com/rdaly525/coreir/blob/master/INSTALL.md
            1. Install from source
            1. Be sure to follow the instructions under the "to build" and "install to /usr or /usr/bin" heading
    1. [PyCoreIR](https://github.com/leonardt/pycoreir)
        1. This provides the Python API for interfacing with CoreIR.
        1. How to install: https://github.com/leonardt/pycoreir#development-setup
    1. [Magma](https://github.com/phanrahan/magma)
        1. This provides the Python API for creating circuits and basic computer architecture components.
        1. How to install: https://github.com/phanrahan/magma#installation
    1. [Mantle](https://github.com/phanrahan/mantle)
        1. This is a library of useful Magma components.
        1. How to install: https://github.com/phanrahan/mantle#setup
1. Install Aetherling:
    ```
    git clone https://github.com/David-Durst/aetherling/
    cd aetherling
    pip install -r requirements.txt
    pip install -e .
    ```
1. Set the following environment variable (if you didn't already do so in the Mantle installation):
    ```
    export MANTLE=coreir
    ```
1. Run the unit tests:
    ```
    pytest -s tests
    ```

