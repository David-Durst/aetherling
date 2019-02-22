#!/usr/local/bin/bash
source activate aetherling
cd coreIR
rm resources.log
touch resources.log
for file_to_build in *.py; do
    python ${file_to_build} &> ${file_to_build}.log
    pcregrep -M '.*_Circuit(\n|.)*\n\n' ${file_to_build}.log >> resources.log
done
