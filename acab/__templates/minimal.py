#!/usr/bin/env python3
"""
A Template for the bare minimum to get a working engine
"""
import acab

config = acab.setup()

from acab.modules.engines.configured import exlo

default_modules = config.attr.Module.REPL.MODULES
eng = exlo()
cls.eng.load_modules(*default_modules)
