#!/usr/local/bin/bash
source activate aetherling
cd verilog
for file_to_build in *.py; do
    python ${file_to_build}
done
scp *.v derp:Trenz/derp/app/