"""
acab is a Production Rule Architecture,
based around a Meta-DSL (Exclusion Logic),
combined with modular DSLs it can host to expand its functionality

network.py provides a core network class and simple protocol to
communicate with unity.

util provides standard enums, strings and some utility functions
"""
import logging as logmod
from typing import Callable, Type, TypeAlias

import acab.interfaces.type_aliases as types
from acab.core.config.config import AcabConfig
from acab.core.util.log_formatter import AcabMinimalLogRecord

logging = logmod.getLogger(__name__)

__all__ = ['types', 'setup', 'AcabConfig', 'AcabLogRecord']

_Value_A : TypeAlias = types.Value
_Sen_A   : TypeAlias = types.Sentence


def setup(location:str=None,
          rich_exc:bool=False,
          format_logs:bool=True,
          ) -> types.Config:
    """
    A Utility to easily setup the config singleton,
    allowing the rest of acab to load.

    Arguments:
        location
            the path to config file(s) to use

        rich_exc
            True for rich.traceback, false for normal python exceptions

        format_logs
            True for AcabLogFormatter installation, if [LOGGING].ACAB is also true

    Returns:
        An initialised Config Object
    """
    #pylint: disable=import-outside-toplevel

    from os.path import join, split

    from acab.core.config.config import AcabConfig
    from acab.core.config.structure_hook import structure_hook
    from acab.core.config.modal_hook import modal_hook
    from acab.core.config.misc_hooks import attr_hook, packrat_hook
    AcabMinimalLogRecord.install()
    if format_logs:
        from acab.core.config.log_hook import log_hook
    else:
        log_hook = lambda x: x

    if location is None or not bool(location):
        base     = split(__file__)[0]
        location = [join(base, "__configs", "default")]
    elif not isinstance(location, list):
        location = [location]

    config = AcabConfig(*location, hooks=[log_hook, modal_hook, attr_hook, packrat_hook])


    if not rich_exc:
        try:
            from rich.traceback import install
            install(show_locals=True)
        except ImportError:
            logging.debug("Rich Module not found, using default exception handler")

    from acab.interfaces.value import ValueFactory
    from acab.core.value.sentence import Sentence
    from acab.core.value.value import AcabValue
    ValueFactory.set(AcabValue, Sentence)

    return config

# TODO provide an easy selection of interfaces
# So... Sentence, parser, engine
