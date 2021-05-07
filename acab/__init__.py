"""
acab is a Production Rule Architecture,
based around a Meta-DSL (Exclusion Logic),
combined with modular DSLs it can host to expand its functionality

network.py provides a core network class and simple protocol to
communicate with unity.

util provides standard enums, strings and some utility functions
"""

def setup():
    from acab.abstract.config.config import AcabConfig
    from os.path import split, join

    base = split(__file__)[0]
    defaults = join(base, "__configs", "default")
    return AcabConfig.Get(defaults)

# TODO provide an easy selection of interfaces
# So... Sentence, parser, engine
