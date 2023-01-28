"""

"""
##-- imports
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
from acab.core.parsing import debug_funcs as DBF
from acab_config.utils.log_formatter import AcabLogFormatter, AcabMinimalLogRecord
from acab.core.value.instruction import ProductionOperator, ProductionStructure
from acab.modules.repl import ReplParser as RP
from acab.modules.repl.repl_commander import register

##-- end imports

config = AcabConfig()


logging = logmod.getLogger(__name__)

# TODO shift this into config
ModuleFragment : TypeAlias = AT.ModuleFragment

SPLIT_RE         = re.compile("[ .!?/]")
shortcut_config  = config.attr.Module.REPL.shortcuts
shortcut_pairs   = sorted([(shortcut_config[cmd], cmd) for cmd in shortcut_config._keys])

@register
def do_parser(self, line):
    """
    obsolete
    """
    params = RP.parse_info_parser.parse_string(line)
    # TODO add * for each spec with debug activated
    if "debug" in params:
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



@register
def do_shortcuts(self, line):
    """
    Print the :{kw} shortcut bindings loaded from config
    """
    print("Repl Shortcut commands: ")
    for kw, cmd in shortcut_pairs:
        print(f"    :{kw:<5} -> {cmd}")


@register
def do_acab(self, line):
    """ All Cops Are Bastards """
    print("All Cops Are Bastards")
