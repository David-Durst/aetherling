# Aetherling
[![Build Status](https://travis-ci.com/David-Durst/aetherling.svg?branch=master)](https://travis-ci.com/David-Durst/aetherling)

## Overview
Aetherling is a library for creating statically scheduled, data-parallel pipelines in hardware. 
This is the Python backend of Aetherling.
The [fronted is in Haskell.](https://github.com/David-Durst/embeddedHaskellAetherling)
The current overview of Aetherling is: https://github.com/David-Durst/embeddedHaskellAetherling/tree/master/theory. 

## Installation
1. Install the following components in the proscribed order:
    1. [CoreIR](https://github.com/rdaly525/coreir/tree/master)
        1. How to install: https://github.com/rdaly525/coreir/blob/master/INSTALL.md
    1. [Fault](https://github.com/leonardt/fault)
        1. How to install:
        ```Shell
            git clone git@github.com:leonardt/fault.git
            cd fault
            pip install -e .
        ```
    1. [Mantle](https://github.com/phanrahan/mantle)
        1. This is a library of useful Magma components.
        1. How to install: https://github.com/phanrahan/mantle#setup
        ```Shell
            git clone git@github.com:leonardt/fault.git
            cd fault
            pip install -e .
        ```
1. Install Aetherling:
    ```Shell
    git clone https://github.com/David-Durst/aetherling/
    cd aetherling
    pip install -e .
    ```
1. Run the unit tests:
    ```Shell
    pytest -s tests
    ```

