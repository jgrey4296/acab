#!/usr/bin/env python3
# pylint: disable=no-member
from __future__ import annotations

import abc
import logging as logmod
from collections import defaultdict
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from os.path import splitext

import pyparsing as pp
from acab import AcabConfig
from acab.modules.repl.repl_commander import register_class
from acab_config.utils.log_formatter import SimpleLogColour
import acab.modules.repl.commands.util as ColPr
from acab.interfaces.fragments import ModuleFragment

logging = logmod.getLogger(__name__)
SC      = SimpleLogColour

config = AcabConfig()
import_dict = config.prepare("Imports.Targeted", _type=dict)

@register_class("log")
class ReportCmd:
    """
    Control the Repl log level, format,
    and specify filters for the logger


    """

    def __init__(self):
        self._parser = self._gen_parser()

    def _gen_parser(self):
        operator_kw  = pp.MatchFirst([pp.Keyword("ops"),
                                      pp.Keyword("operator")])("operator")
        module_kw    = pp.MatchFirst([pp.Keyword("mod"),
                                      pp.Keyword("module")])("module")
        semantic_kw  = pp.Keyword("semantics")("semantics")
        printer_kw   = pp.Keyword("printers")("printers")
        dsl_kw       = pp.Keyword("dsl")("dsl")
        engine_kw    = pp.Keyword("engine")("engine")

        eol          = pp.line_end("all")

        report_parser = pp.ZeroOrMore(pp.MatchFirst([operator_kw,
                                                    module_kw,
                                                    semantic_kw,
                                                    printer_kw,
                                                    dsl_kw,
                                                    engine_kw,
                                                    eol]))
        return report_parser

    def __call__(self, line):
        self.do_report(line)

    def _log_level(self, line):
        """
        Change the console logging level

        Usage:
        log {DEBUG | INFO | WARN | ERROR}
        log
        """
        try:
            root = logmod.getLogger('')
            handler = [x for x in root.handlers if not isinstance(x, logmod.FileHandler)][0]
            if bool(line):
                level = logmod._nameToLevel[line.upper()]
                if root.level > level:
                    logging.warning("Logging Root level is masking the handler level")
                handler.setLevel(level)
                print(f"Set Console Log level to: {line.upper()} : {level}")
            else:
                level = handler.level
                if handler.level in logmod._levelToName:
                    level = logmod._levelToName[handler.level]
                print(f"Console Log Level: {level}")

        except KeyError as err:
            print(f"Unrecognised Log Level: {line.upper()}")

    def _log_fmt(self, line):
        """
        Change the console Log format.
        The Repl Uses AcabLogFormatter and AcabLogRecord by default,
        which uses {} style format strings for the log format
        """
        root = logmod.getLogger('')
        handler = [x for x in root.handlers if not isinstance(x, logmod.FileHandler)][0]
        if bool(line):
            handler.setFormatter(AcabLogFormatter(fmt=line, record=True))
            print(f"Set Console Log Format to: {line}")
        else:
            fmt = handler.formatter._fmt
            recordFactory = logmod.getLogRecordFactory()
            print(f"Console Log Level: {fmt}")
            if hasattr(recordFactory, "available_fields"):
                print(f"Availble Log Fields: {recordFactory.available_fields}")
            else:
                print("For Available Fields see:",
                    "https://docs.python.org/3/library/logging.html#logrecord-attributes")

    def _log_filter(self, line):
        """ Add or remove a logging filter """
        root = logmod.getLogger('')
        handler = root.handlers[1]
        if bool(line):
            handler.addFilter(logmod.Filter(line))
            print(f"Set Console Log Format to: {line}")
        elif bool(handler.filters):
            handler.removeFilter(handler.filters[-1])
            print(f"Removing Last Console Log Filter")
        else:
            print("No filters set")
