#!/usr/bin/env python

from setuptools import setup

setup(
    name="mkbip39",
    version="0.5",
    description="Make a seed phrase",
    author="mbrock",
    url="https://github.com/dapphub/mkbip39",
    install_requires=["mnemonic"],
    scripts=["mkbip39"]
)
