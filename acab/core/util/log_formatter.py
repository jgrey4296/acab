#!/usr/bin/env python3
# from https://alexandra-zaharia.github.io/posts/make-your-own-custom-color-formatter-with-python-logging/
from __future__ import annotations

import logging
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from collections import defaultdict
from string import Formatter
import _string

try:
    from sty import fg, rs, bg, ef
    COLOUR_RESET = rs.all
    LEVEL_MAP = {
        logging.DEBUG    : fg.grey,
        logging.INFO     : fg.blue,
        logging.WARNING  : fg.yellow,
        logging.ERROR    : fg.red,
        logging.CRITICAL : fg.red,
        "blue"           : fg.blue,
        "cyan"           : fg.cyan,
        "green"          : fg.green,
        "magenta"        : fg.magenta,
        "red"            : fg.red,
        "yellow"         : fg.yellow,
        "bg_blue"        : bg.blue,
        "bg_cyan"        : bg.cyan,
        "bg_green"       : bg.green,
        "bg_magenta"     : bg.magenta,
        "bg_red"         : bg.red,
        "bg_yellow"      : bg.yellow,
        "bold"           : ef.bold,
        "underline"      : ef.u,
        "italic"         : ef.italic,
        "RESET"          : rs.all
        }
except ImportError:
    LEVEL_MAP    = defaultdict(lambda: "")
    COLOUR_RESET = ""

class SimpleLogColour:
    def green(s):
        return LEVEL_MAP['green'] + str(s) + COLOUR_RESET

    def blue(s):
        return LEVEL_MAP['blue'] + str(s) + COLOUR_RESET

    def yellow(s):
        return LEVEL_MAP['yellow'] + str(s) + COLOUR_RESET

    def red(s):
        return LEVEL_MAP['red'] + str(s) + COLOUR_RESET


class AcabStringFormatter(Formatter):
    """
    Custom String Formatter to be able to specify colours
    within the format spec.
    see https://docs.python.org/3/library/string.html
    The Resulting dsl is:

    format_spec     ::=  [[fill]align][sign][#][0][width][grouping_option][.precision][type][colour]
    fill            ::=  <any character>
    align           ::=  "<" | ">" | "=" | "^"
    sign            ::=  "+" | "-" | " "
    width           ::=  digit+
    grouping_option ::=  "_" | ","
    precision       ::=  digit+
    type            ::=  "b" | "c" | "d" | "e" | "E" | "f" | "F" | "g" | "G" | "n" | "o" | "s" | "x" | "X" | "%"
    colour          ::=  "&" colour
    """
    def __init__(self):
        super().__init__()
        self._colour = True

    def format(self, msg, *args, colour=True, **kwargs):
        self._colour = colour
        return super().format(msg, *args, **kwargs)


    def format_field(self, value, format_spec):
        split_format = format_spec.split("&")
        result = format(value, split_format[0])
        # apply colour
        if (self._colour and
            len(split_format) == 2 and
            split_format[1] in LEVEL_MAP):
            result = LEVEL_MAP[split_format[1]] + result + COLOUR_RESET
        return result



class AcabMinimalLogRecord(logging.LogRecord):
    """
    Acab uses {} formatting in log messages,
    this is a minimal log record modification to handle that
    """

    @classmethod
    def install(cls):
        """
        Install the log record, capable of handling
        both % and .format style messages
        """
        logging.warning("Installing %s", cls.__name__)
        logging.setLogRecordFactory(cls)

    def getMessage(self, colour=True):
        """
        Minimal modification of default getMessage,
        which on a type error for: record.msg % record.args
        will try: record.msg.format(*record.args)
        """
        msg = str(self.msg)
        if self.args:
            try:
                msg = msg % self.args
            except TypeError:
                msg = msg.format(*self.args)

        return msg


class AcabLogRecord(AcabMinimalLogRecord):
    """
    Custom LogRecord which provides
    \.shortname : attribute of just the last 25 characters of the logging path
    (for aligning log files nicely)

    % and {} style formatting of the log message, using AcabStringFormatter.
    (this also allows &{colour} format spec)
    """

    fmt = AcabStringFormatter()

    @classmethod
    def install(cls):
        """
        Install the log record, capable of handling
        both % and .format style messages
        """
        logging.warning("Installing %s", cls.__name__)
        logging.setLogRecordFactory(cls)

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.shortname = ".".join(self.name.split(".")[-2:])[-25:]

    def getMessage(self, colour=True):
        msg = str(self.msg)
        if self.args:
            try:
                msg = msg % self.args
            except TypeError:
                if hasattr(self, 'ctx'):
                    msg = self.fmt.format(msg, *self.args, colour=colour, ctx=self.ctx)
                else:
                    msg = self.fmt.format(msg, *self.args, colour=colour)

        msg = msg.replace('\t', '    ')
        return msg



class AcabLogFormatter(logging.Formatter):
    """
    Stream Formatter for acab, enables use of colour sent to console

    Guarded Formatter for adding colour.
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

    def __init__(self, *, fmt=None, record=False):
        """
        Create the AcabLogFormatter with a given *Brace* style log format
        `record` will install the AcabLogRecord as the record factory if true
        """
        if not issubclass(logging.getLogRecordFactory(), AcabMinimalLogRecord):
            AcabMinimalLogRecord.install()

        super().__init__(fmt or self._default_fmt,
                         datefmt=self._default_date_fmt,
                         style=self._default_style)
        self.colours = LEVEL_MAP

    def format(self, record):
        log_colour = self.colours.get(record.levelno)
        return log_colour + super().format(record) + COLOUR_RESET



class AcabNameTruncateFormatter(logging.Formatter):
    """
    A Formatter for file logging, in a more verbose format than the stream formatter
    """
    _default_fmt      = "{asctime} | {levelname:9} | {shortname:25} | {message}"
    _default_date_fmt = "%Y-%m-%d %H:%M:%S"
    _default_style    = '{'

    def __init__(self, *, fmt=None):
        if not issubclass(logging.getLogRecordFactory(), AcabLogRecord):
            AcabLogRecord.install()

        super().__init__(fmt=fmt or self._default_fmt,
                         datefmt=self._default_date_fmt,
                         style=self._default_style)

    def format(self, record):
        """
        Format the specified record as text.

        A Simple modification of the default logging.Formatter.format method,
        to ensure colours aren't generated by the AcabLogRecord

        """
        if isinstance(record, AcabLogRecord):
            record.message = record.getMessage(colour=False)
        else:
            record.message = record.getMessage()

        if self.usesTime():
            record.asctime = self.formatTime(record, self.datefmt)
        s = self.formatMessage(record)
        if record.exc_info:
            # Cache the traceback text to avoid converting it multiple times
            # (it's constant anyway)
            if not record.exc_text:
                record.exc_text = self.formatException(record.exc_info)
        if record.exc_text:
            if s[-1:] != "\n":
                s = s + "\n"
            s = s + record.exc_text
        if record.stack_info:
            if s[-1:] != "\n":
                s = s + "\n"
            s = s + self.formatStack(record.stack_info)
        return s
