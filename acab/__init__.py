"""
acab is a Production Rule Architecture,
based around a Meta-DSL (Exclusion Logic),
combined with modular DSLs it can host to expand its functionality

network.py provides a core network class and simple protocol to
communicate with unity.

util provides standard enums, strings and some utility functions
"""
import acab.interfaces.types as types
from acab.core.config.config import GET

def setup(location=None):
    from acab.core.config.config import AcabConfig
    from acab.core.config.modal import modal_config
    import acab.core.config.structure
    from os.path import split, join

    if location is None:
        base = split(__file__)[0]
        location = join(base, "__configs", "default")
    return AcabConfig.Get(location, hooks=[modal_config])

# TODO provide an easy selection of interfaces
# So... Sentence, parser, engine
