from setuptools import setup

with open("README.md", "r") as fh:
    LONG_DESCRIPTION = fh.read()

DESCRIPTION = """\
A Python package for testing hardware (part of the magma ecosystem)\
"""

setup(
    name='aetherling',
    version='0.0.2',
    description='Aetherling: a DSL for compiling data-parallel programs to hardware accelerators.',
    packages=["aetherling"],
    install_requires=[
        "magma-lang>=1.0.18",
        "coreir",
        "fault",
        "mantle",
        "numpy",
        "Pillow",
        "bitarray",
        "colorlog",
        "pytest"
    ],
    license='BSD License',
    url='https://github.com/David-Durst/aetherling',
    author='David Durst',
    python_requires='>=3.7',
    author_email='davidbdurst@gmail.com',
    long_description=LONG_DESCRIPTION,
    long_description_content_type="text/markdown"
)
