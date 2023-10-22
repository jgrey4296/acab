#!/usr/bin/env python3
"""
These are signals the print system uses, in addition to any specific
type printers
"""
##-- imports
from __future__ import annotations

import abc
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

import  acab
from acab.interfaces.value import ValueFactory as VF

##-- end imports

config = acab.config

signal_dict = {x:x for x in config.all_of().print.signals.allowed()}

# Print signals with no semantic component are Str's
TYPE_INSTANCE      = signal_dict.get("TYPE_INSTANCE", "TYPE_INSTANCE")
MODAL              = signal_dict.get("MODAL", "MODAL")
SYMBOL             = signal_dict.get("SYMBOL", "SYMBOL")
ANNOTATIONS        = signal_dict.get("ANNOTATIONS", "ANNOTATIONS")
ANNOTATIONS_FINAL  = signal_dict.get("ANNOTATIONS_FINAL", "ANNOTATIONS_FINAL")
CONSTRAINT         = signal_dict.get("CONSTRAINT", "CONSTRAINT")
IMPLICIT_CONTAINER = signal_dict.get("IMPLICIT_CONTAINER", "IMPLICIT_CONTAINER")
TAGS               = signal_dict.get("TAGS", "TAGS")

# Signals with semantic/type relevance are Sentences
INSTR              = VF.sen() << config.types.primitive.INSTRUCT
CONTAINER          = INSTR << config.types.primitive.CONTAINER
STRUCTURE          = INSTR << config.types.primitive.STRUCTURE

SENTENCE           = VF.sen() << config.types.primitive.SENTENCE
COMPONENT          = SENTENCE << config.types.primitive.COMPONENT

ATOM               = VF.sen() << config.data.TYPE_BASE
