#!/usr/bin/env python3
from __future__ import annotations

import abc
import logging as logmod
from copy import deepcopy
from dataclasses import InitVar, dataclass, field
from os.path import splitext
from re import Pattern
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from uuid import UUID, uuid1
from weakref import ref

from acab_config.utils.log_formatter import SimpleLogColour

logging = logmod.getLogger(__name__)
SC      = SimpleLogColour

def print_header(header):
    print("\n--------------------")
    print(f"{SC.green(header)}: ")
    print("--------------------")

def print_class_colour(heading, cls):
    print(f"{heading}: {SC.yellow(cls.__module__)}.{SC.green(cls.__class__.__name__)}")
    if bool(cls.__doc__):
        print(SC.blue(cls.__doc__))

    print()
    bases = "\n\t".join(f"{SC.yellow(x.__module__)}.{SC.green(x.__qualname__)}" for x in cls.__class__.__bases__)
    print(f"Instance of:\n\t{bases}")


def print_module_colour(mod):
    result = []
    if bool(len(mod.dsl_fragments)):
        result.append(SC.green(f"{len(mod.dsl_fragments)} DSL"))
    else:
        result.append(SC.red(f"{len(mod.dsl_fragments)} DSL"))
    if bool(len(mod.semantics)):
        result.append(SC.green(f"{len(mod.semantics)} Sem"))
    else:
        result.append(SC.red(f"{len(mod.semantics)} Sem"))
    if bool(len(mod.operators)):
        result.append(SC.green(f"{len(mod.operators)} Op"))
    else:
        result.append(SC.red(f"{len(mod.operators)} Op"))
    if bool(len(mod.printers)):
        result.append(SC.green(f"{len(mod.operators)} Pr"))
    else:
        result.append(SC.red(f"{len(mod.operators)} Pr"))

    source = splitext(mod.source)
    print(f"\t({' | '.join(result)} : {source[0]}{SC.blue(source[1])})")


def print_handler_system(system):
    print("\nComponents: (no. of handlers : signal : flags)")
    sorted_specs = sorted([(len(y), y, x) for x,y in system.handler_specs.items()], reverse=True, key=lambda x: x[0])
    for spec_len, spec, signal in sorted_specs:
        spec_len = SC.green(f"{spec_len:<4}") if bool(spec_len) else SC.red(f"{spec_len:<4}")
        spec_flags = [x.name for x in spec.flags]
        flag_str = f" : {', '.join(spec_flags)}" if bool(spec_flags) else ""
        print(f"\t{spec_len} : {SC.blue(signal):<40}{flag_str}")

    print("\nHandlers not attached to a Signal: ")
    for signal in sorted((x.signal for x in system.loose_handlers)):
        print(f"\t{SC.blue(signal)}")


def two_part_split(the_string):
    pair = splitext(the_string)
    return f"{pair[0]}{SC.blue(pair[1])}"
