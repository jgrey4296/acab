"""
acab is a Production Rule Architecture,
based around a Meta-DSL (Exclusion Logic),
combined with modular DSLs it can host to expand its functionality

network.py provides a core network class and simple protocol to
communicate with unity.

util provides standard enums, strings and some utility functions
"""
import acab.abstract.interfaces.types as types
from acab.abstract.config.config import GET

def setup(location=None):
    from acab.abstract.config.config import AcabConfig
    from acab.abstract.config.modal import modal_config
    import acab.abstract.config.structure
    from os.path import split, join

    if location is None:
        base = split(__file__)[0]
        location = join(base, "__configs", "default")
    return AcabConfig.Get(location, hooks=[modal_config])

# TODO provide an easy selection of interfaces
# So... Sentence, parser, engine
