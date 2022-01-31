#!/usr/bin/env python

from re import Pattern

from acab.core.config.config import AcabConfig
import acab.interfaces.value as VI

config           = AcabConfig.Get()
ANON_VALUE       = config.prepare("Symbols", "ANON_VALUE")()

name_sieve_fns = [
    lambda x: x.name,
    lambda x: x.__class__.__name__ if x.name is None and x.value is None else None,
    lambda x: x.value.pattern if isinstance(x.value, Pattern) else None,
    lambda x: ANON_VALUE if isinstance(x.value, (list, VI.Instruction_i)) else None,
    lambda x: str(x.value) if x.value is not None else None,
    lambda x: ANON_VALUE
    ]
