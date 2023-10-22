"""

"""
##-- imports
from __future__ import annotations

import abc
import importlib
import logging as logmod
import re
import tracemalloc
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
from acab import types as AT
from acab.core.parsing import debug_funcs as DBF
from acab.core.util.debugging import (human, print_diff, print_stat,
                                      print_stat_file, sh_filter)
from acab.core.value.instruction import ProductionOperator, ProductionStructure
from acab.modules.repl import ReplParser as RP
from acab.modules.repl.repl_commander import register, register_default

##-- end imports

logging = logmod.getLogger(__name__)
config  = acab.config

snap_a  = None

def display_memory(self):
    """
    A Post Command Hook for ReplCommander, to print the current and peak memory
    usage.
    """
    global snap_a
    traced  = tracemalloc.get_traced_memory()
    print("Memory: Current: {:<5}, Peak: {:<5}".format(human(traced[0]), human(traced[1])))

@register
def do_memdiff(self, line):
    """
    Print the top 20 differences between the last snapshot and now
    """
    current = tracemalloc.take_snapshot().filter_traces(sh_filter)
    diff    = current.compare_to(snap_a, 'lineno')
    for x in diff[:20]:
        print_diff(x)

@register
def do_memstat(self, line):
    """
    Print the top 20 File based memory statistics
    """
    if not tracemalloc.is_tracing():
        print("Call memstat after starting memory tracing with `memory`")
        return
    current = tracemalloc.take_snapshot().filter_traces(sh_filter)
    print("\nBy Filename --------------------")
    for stat in current.statistics("filename")[:20]:
        print_stat_file(stat)

    print()

@register
def do_snapshot(self, line):
    """
    Take a Memory Snapshot for comparison
    """
    global snap_a, snap_b
    if not tracemalloc.is_tracing():
        print("Must start memory tracing to take a snapshot")
        print("Call `memory`")
        return

    snap_a = tracemalloc.take_snapshot().filter_traces(sh_filter)


@register
def do_memory(self, line):
    """
    Toggle Memory Tracing
    """
    limit = 50
    if display_memory.__name__ in self.state.post_cmds:
        print("Disabling Memory Trace")
        del self.state.post_cmds[display_memory.__name__]
        tracemalloc.stop()
        return

    print(f"Adding Memory Trace with limit {limit}")
    tracemalloc.start(limit)
    do_snapshot(self, "")
    self.state.post_cmds[display_memory.__name__] = display_memory


def ctx_prompt(self):
    """
    Post Command Hook for ReplCommander,
    which inserts the count of active contexts
    """
    count = "0"
    if self.state.ctxs is not None:
        count = len(self.state.ctxs)
    insert = f"(Γ: {count})"

    self.prompt = f"{self.state.prompt} {insert}: "

@register_default
def do_ctxprompt(self, line):
    """
    Turn On or Off the prompt notification of the active context amount
    """
    hook_name = "a_" + ctx_prompt.__name__
    if hook_name not in self.state.post_cmds:
        self.state.post_cmds[hook_name] = ctx_prompt
    else:
        del self.state.post_cmds[hook_name]
        self.prompt = f"{self.state.prompt}: "

