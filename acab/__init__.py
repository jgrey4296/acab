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
from acab.core.config.config import GET
from acab.core.util.log_formatter import AcabLogFormatter, AcabLogRecord

logging = logmod.getLogger(__name__)

__all__ = ['types', 'setup', 'GET']

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
            True for AcabLogFormatter installation

    Returns:
        An initialised Config Object
    """
    #pylint: disable=import-outside-toplevel

    from os.path import join, split

    if format_logs:
        root_logger = logmod.getLogger()
        stream_handler = [x for x in root_logger.handlers if isinstance(x, logmod.StreamHandler)]
        if bool(stream_handler):
            stream_handler[0].setFormatter(AcabLogFormatter())

    if logmod.getLogRecordFactory is not AcabLogRecord:
        AcabLogRecord.install()

    import acab.core.config.structure
    from acab.core.config.config import AcabConfig
    from acab.core.config.modal import modal_config

    if location is None or not bool(location):
        base     = split(__file__)[0]
        location = [join(base, "__configs", "default")]
    elif not isinstance(location, list):
        location = [location]

    config = AcabConfig(*location, hooks=[modal_config])


    if not rich_exc:
        try:
            from rich.traceback import install
            install(show_locals=True)
        except ImportError:
            logging.debug("Rich Module not found, using default exception handler")

    from acab.interfaces.value import ValueFactory_i
    from acab.core.data.sentence import Sentence
    from acab.core.data.value import AcabValue
    ValueFactory_i.set(AcabValue, Sentence)

    return config

# TODO provide an easy selection of interfaces
# So... Sentence, parser, engine
