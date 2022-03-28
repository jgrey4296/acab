#!/usr/bin/env python3
# from https://alexandra-zaharia.github.io/posts/make-your-own-custom-color-formatter-with-python-logging/
from __future__ import annotations

import logging
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from collections import defaultdict

try:
    from sty import fg
    COLOUR_RESET = fg.rs
    LEVEL_MAP = {
        logging.DEBUG    : fg.grey,
        logging.INFO     : fg.blue,
        logging.WARNING  : fg.yellow,
        logging.ERROR    : fg.red,
        logging.CRITICAL : fg.red,
    }
    COLOUR_MAP = {
        "green"  : fg.green,
        "blue"   : fg.blue,
        "yellow" : fg.yellow,
        "red"    : fg.red
        }

except ImportError:
    COLOUR_RESET = ""
    LEVEL_MAP = defaultdict(lambda: "")
    COLOUR_MAP = LEVEL_MAP

class SimpleColour:

    def green(s):
        return COLOUR_MAP['green'] + str(s) + COLOUR_RESET

    def blue(s):
        return COLOUR_MAP['blue'] + str(s) + COLOUR_RESET

    def yellow(s):
        return COLOUR_MAP['yellow'] + str(s) + COLOUR_RESET

    def red(s):
        return COLOUR_MAP['red'] + str(s) + COLOUR_RESET

class AcabParseDebugFormat(logging.Formatter):
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

    _default_fmt : ClassVar[str] = '%(message)s'

    _total = "%(name)s %(levelno)s %(levelname)s %(pathname)s %(filename)s %(module)s %(lineno)d %(funcName)s %(created)f %(asctime)s %(msecs)d %(relativeCreated)d %(thread)d %(threadName)s %(process)d %(message)s"


    @staticmethod
    def scaffold():
        handler = logging.StreamHandler()
        handler.setLevel(logging.DEBUG)
        handler.setFormatter(AcabParseDebugFormat())
        return handler

    def __init__(self, fmt=None):
        super().__init__(fmt or AcabParseDebugFormat._default_fmt)
        self.levels = LEVEL_MAP
        self.datefmt : str = "%H:%M:%S"

    def format(self, record):
        record.msg = record.msg.replace("\t", "    ")
        # log_colour = self.levels.get(record.levelno)
        formatted = super().format(record)

        return formatted + COLOUR_RESET
