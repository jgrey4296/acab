#!/usr/bin/env python3
# from https://alexandra-zaharia.github.io/posts/make-your-own-custom-color-formatter-with-python-logging/
from __future__ import annotations

import logging
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

from acab.core.util.log_formatter import (COLOUR_RESET, LEVEL_MAP,
                                          AcabLogFormatter, AcabLogRecord,
                                          SimpleLogColour)


class AcabParseDebugFormat(AcabLogFormatter):
    """ Guarded Formatter for adding colour.
    Uses the sty module.
    If sty is missing, behaves as the default formatter class

    # Do *not* use for on filehandler
    Usage reminder:
    # Create stdout handler for logging to the console (logs all five levels)
    stdout_handler = logging.StreamHandler()
    stdout_handler.setFormatter(AcabParseDebugFormat(fmt))
    logger.addHandler(stdout_handler)
    """

    _default_fmt : ClassVar[str] = '{message}'

    _total = "%(name)s %(levelno)s %(levelname)s %(pathname)s %(filename)s %(module)s %(lineno)d %(funcName)s %(created)f %(asctime)s %(msecs)d %(relativeCreated)d %(thread)d %(threadName)s %(process)d %(message)s"


    @staticmethod
    def scaffold():
        """
        Setup the AcabParseDebugFormat logging scaffold,
        a specific debug level handler with ParseDebug formatting,
        which is attached to acab.core.parsing.debug_funcs
        """
        logging.warning("Scaffolding AcabParseDebugFormat")
        if not isinstance(logging.getLogRecordFactory(), AcabLogRecord):
            AcabLogRecord.install()

        handler = logging.StreamHandler()
        handler.setLevel(logging.WARNING)
        handler.setFormatter(AcabParseDebugFormat())
        return handler

    def __init__(self, fmt=None):
        super().__init__(fmt=fmt or AcabParseDebugFormat._default_fmt, record=True)
        self.levels = LEVEL_MAP
        self.datefmt : str = "%H:%M:%S"

    def format(self, record):
        # record.msg = record.msg.replace("\t", "    ")
        # log_colour = self.levels.get(record.levelno)
        formatted = super().format(record)

        return formatted + COLOUR_RESET
