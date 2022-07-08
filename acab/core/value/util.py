"""

"""
from __future__ import annotations

from re import Pattern

import acab.interfaces.value as VI
from acab import AcabConfig

config           = AcabConfig()
ANON_VALUE       = config.attr.Symbols.ANON_VALUE

def class_name_fn(obj):
    if obj['name'] is None and obj['value'] is None:
        # class_path = obj['class'].__module__.split(".")[-1]
        pattern = f"{obj['class'].__name__}"
        return pattern
    else:
        return None


name_sieve_fns = [
    lambda x: x['name'],
    class_name_fn,
    lambda x: x['value'].pattern if isinstance(x['value'], Pattern) else None,
    # lambda x: '"{}"'.format(".".join([str(y) for y in x['value']])) if isinstance(x['value'], list) else None,
    lambda x: ANON_VALUE if isinstance(x['value'], list) else None,
    lambda x: ANON_VALUE if isinstance(x['value'], VI.Instruction_i) else None,
    lambda x: str(x['value']) if x['value'] is not None else None,
    lambda x: ANON_VALUE
    ]

