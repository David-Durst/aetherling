from setuptools import setup
from pip.req import parse_requirements

# parse_requirements() returns generator of pip.req.InstallRequirement objects

install_requires = []
extra_requires = {}
for item in parse_requirements("requirements.txt", session=False):
    req = str(item.req)
    if item.markers is not None:
        req += ";" + str(item.markers)
    install_requires.append(req)

setup(
    name='aetherling',
    version='0.0.1',
    description='The python component of Aetherling\'s, the library for data-parallel programming using the Stanford coreir and magma projects',
    packages=["aetherling"],
    license='BSD License',
    url='https://github.com/David-Durst/aetherling',
    author='David Durst',
    author_email='durst@cs.stanford.edu'
)
