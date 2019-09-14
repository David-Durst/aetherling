#!/bin/bash
mkdir -p dist
rm dist/*
python3 setup.py sdist bdist_wheel
twine upload dist/*