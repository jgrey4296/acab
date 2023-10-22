"""
acab is a Production Rule Architecture,
based around a Meta-DSL (Exclusion Logic),
combined with modular DSLs it can host to expand its functionality

network.py provides a core network class and simple protocol to
communicate with unity.

util provides standard enums, strings and some utility functions
"""
##-- imports
from __future__ import annotations

from importlib.resources import files
import logging as logmod
from os.path import join, split
from typing import Callable, Type, TypeAlias

import acab.interfaces.type_aliases as types
from tomler  import Tomler
import acab_config

##-- end imports

logging = logmod.getLogger(__name__)

__version__ = "0.0.1"
__all__     = ['types', 'setup', 'AcabLogRecord']

_Value_A : TypeAlias = types.Value
_Sen_A   : TypeAlias = types.Sentence

config   : Tomler    = None

def setup(location:str|list[str]=None, rich_exc:bool=False, format_logs:bool=True,):
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
    global config

    acab_config.AcabMinimalLogRecord.install()

    match location:
        case None | False | []:
            tomls  = list((files("acab.__configs") / "toml").glob("*.toml"))
            config = Tomler.load(*tomls)
        case [*tomls]:
            config = Tomler.load(*tomls)
        case pl.Path() if location.is_dir():
            config = Tomler.load_dir(location)
        case _:
            raise Exception("Unknown location for loading toml config")

    if format_logs:
        from acab_config.hooks.log_hook import log_hook
    else:
        log_hook = lambda x: x

    if rich_exc or config.logging.rich_exceptions:
        try:
            from rich.traceback import install
            install(show_locals=True)
        except ImportError:
            logging.debug("Rich Module not found, using default exception handler")

    from acab.interfaces.value import ValueFactory
    from acab.core.value.value import AcabValue
    from acab.core.value.sentence import Sentence
    ValueFactory.set(AcabValue, Sentence)

    return config

# TODO provide an easy selection of interfaces
# So... Sentence, parser, engine
