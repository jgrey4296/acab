"""
acab is a Production Rule Architecture,
based around a Meta-DSL (Exclusion Logic),
combined with modular DSLs it can host to expand its functionality

network.py provides a core network class and simple protocol to
communicate with unity.

util provides standard enums, strings and some utility functions
"""
from typing import Callable, Type, TypeAlias
import acab.interfaces.type_aliases as types
from acab.core.config.config import GET
import logging as root_logger
logging = root_logger.getLogger(__name__)

__all__ = ['types', 'setup', 'GET']

_Value_A : TypeAlias = types.Value
_Sen_A   : TypeAlias = types.Sentence

test : str = 20

def setup(location=None, basic_exc=False, custom_factory_fns:None|tuple[Type[_Value_A], Type[_Sen_A]]=None):
    """
    A Utility to easily setup the config system,
    allowing the rest to load.
    """
    #pylint: disable=import-outside-toplevel
    from acab.core.config.config import AcabConfig
    from acab.core.config.modal import modal_config
    import acab.core.config.structure
    from os.path import split, join

    if location is None:
        base = split(__file__)[0]
        location = join(base, "__configs", "default")

    config = AcabConfig(location, hooks=[modal_config])

    from acab.interfaces.value import ValueFactory_i
    if not custom_factory_fns:
        from acab.core.data.value import AcabValue
        from acab.core.data.sentence import Sentence
        ValueFactory_i.set(AcabValue, Sentence)
    else:
        ValueFactory_i.set(*custom_factory_fns)

    return config

# TODO provide an easy selection of interfaces
# So... Sentence, parser, engine
