#!/usr/bin/env python3
# from https://alexandra-zaharia.github.io/posts/make-your-own-custom-color-formatter-with-python-logging/
from __future__ import annotations

import logging
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

try:
    from sty import fg
    COLOUR_RESET = fg.rs
    COLOUR_MAP = {
        logging.DEBUG    : fg.grey,
        logging.INFO     : fg.blue,
        logging.WARNING  : fg.yellow,
        logging.ERROR    : fg.red,
        logging.CRITICAL : fg.red,
    }
except ImportError:
    COLOUR_RESET = ""
    COLOUR_MAP = {"__missing": "sty module"}


class AcabLogFormatter(logging.Formatter):
    """ Guarded Formatter for adding colour.
    Uses the sty module.
    If sty is missing, behaves as the default formatter class

    # Do *not* use for on filehandler
    Usage reminder:
    # Create stdout handler for logging to the console (logs all five levels)
    stdout_handler = logging.StreamHandler()
    stdout_handler.setFormatter(AcabLogFormatter(fmt))
    logger.addHandler(stdout_handler)
    """

    _default_fmt : ClassVar[str] = '%(asctime)s | %(message)s'

    _total = "%(name)s %(levelno)s %(levelname)s %(pathname)s %(filename)s %(module)s %(lineno)d %(funcName)s %(created)f %(asctime)s %(msecs)d %(relativeCreated)d %(thread)d %(threadName)s %(process)d %(message)s"

    def __init__(self, fmt=None):
        super().__init__(fmt or AcabLogFormatter._default_fmt)
        self.colours = COLOUR_MAP
        self.datefmt : str = "%H:%M:%S"

    def format(self, record):
        if "__missing" in self.colours:
            return super().format(record)

        record.msg = record.msg.replace("\t", "    ")
        log_colour = self.colours.get(record.levelno)
        return log_colour + super().format(record) + fg.rs
