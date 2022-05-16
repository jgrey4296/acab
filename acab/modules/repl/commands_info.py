from __future__ import annotations

import abc
import importlib
import logging as logmod
import re
from collections import defaultdict
from dataclasses import InitVar, dataclass, field
from datetime import datetime
from enum import Enum
from os.path import abspath, exists, expanduser, split, splitext
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

import acab
import pyparsing as pp
from acab import AcabConfig
from acab import types as AT

config = AcabConfig()

from acab.core.parsing import debug_funcs as DBF
from acab.core.value.instruction import ProductionOperator, ProductionStructure
from acab.modules.repl import ReplParser as RP
from acab.modules.repl.repl_commander import register
from acab.core.util.log_formatter import AcabLogFormatter, AcabMinimalLogRecord

logging = logmod.getLogger(__name__)

# TODO shift this into config
ModuleFragment : TypeAlias = AT.ModuleFragment

SPLIT_RE         = re.compile("[ .!?/]")
shortcut_config  = config.attr.Module.REPL.shortcuts
shortcut_pairs   = sorted([(shortcut_config[cmd], cmd) for cmd in shortcut_config._keys])

@register
def do_parser(self, line):
    """ Print a parser report.
    Defaults to primary, can take:
    handlers,
    sugar
    """
    params = RP.parse_info_parser.parse_string(line)
    # TODO add * for each spec with debug activated
    if "signals" in params:
        print("DSL Signal Handlers:")
        bootstrap_desc = self.state.engine._dsl.handler_specs.keys()
        for sen in bootstrap_desc:
            print("\t", self.state.engine.pprint(target=[sen]))
    elif "sugar" in params:
        print(f"Repl Sugar: {RP.sugared}")

    elif "debug" in params:
        # TODO enable control of entry/success/fail debug funcs
        if not bool(params['debug']):
            DBF.debug_pyparsing()
            return
        debug_param = params['debug'].strip()
        if debug_param == "disable":
            self.state.engine._dsl.disable_debugs()
            print("Parser Debuggers Cleared")
        elif debug_param == "list":
            active = self.state.engine._dsl.active_debugs()
            if not bool(active):
                print("No Parsers are set for Debugging")
            else:
                print("Debugging Parser Signals:\n\t -- {}".format("\n\t -- ".join(active)))
        else:
            result = self.state.engine._dsl.debug_parser(debug_param)
            print(f"Debugging Signal <{debug_param}> : {result}")
    else:
        print("Top Level ACAB Parser:")
        print(self.state.engine._dsl.lookup())



@register
def do_shortcuts(self, line):
    """
    Print the :{kw} shortcut bindings loaded from config
    """
    print("Repl Shortcut commands: ")
    for kw, cmd in shortcut_pairs:
        print(f"    :{kw:<5} -> {cmd}")


# Logging Control###############################################################
@register
def do_log(self, line):
    """ Change the console logging level """
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

@register
def do_fmt(self, line):
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

@register
def do_filter(self, line):
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


@register
def do_acab(self, line):
    print("All Cops Are Bastards")
