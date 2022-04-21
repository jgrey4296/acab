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


class AcabLogRecord(logging.LogRecord):

    @classmethod
    def install(cls):
        """
        Install the log record, capable of handling
        both % and .format style messages
        """
        logging.setLogRecordFactory(cls)

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.shortname = ".".join(self.name.split(".")[-2:])[-25:]

    def getMessage(self):
        msg = str(self.msg)
        if self.args:
            try:
                msg = msg % self.args
            except TypeError:
                msg = msg.format(*self.args)

        msg = msg.replace('\t', '    ')
        return msg



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

    _default_fmt : ClassVar[str] = '{asctime} | {levelname:9} | {message}'
    _default_date_fmt : str      =  "%H:%M:%S"
    _default_style               = '{'

    _fields= "name levelno levelname pathname filename module lineno funcName created asctime msecs relativeCreated thread threadName process message"

    def __init__(self, fmt=None):
        if logging.getLogRecordFactory is not AcabLogRecord:
            AcabLogRecord.install()
        super().__init__(fmt or self._default_fmt,
                         datefmt=self._default_date_fmt,
                         style=self._default_style)
        self.colours = COLOUR_MAP

    def format(self, record):
        if "__missing" in self.colours:
            return super().format(record)

        log_colour = self.colours.get(record.levelno)
        return log_colour + super().format(record) + COLOUR_RESET



class AcabNameTruncateFormatter(logging.Formatter):
    _default_fmt      = "{asctime} | {levelname:9} | {shortname:25} | {message}"
    _default_date_fmt = "%Y-%m-%d %H:%M:%S"
    _default_style    = '{'

    def __init__(self):
        if logging.getLogRecordFactory is not AcabLogRecord:
            AcabLogRecord.install()
        super().__init__(self._default_fmt, self._default_date_fmt, style=self._default_style)
