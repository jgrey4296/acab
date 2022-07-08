#!/usr/bin/env python3
"""
These are signals the print system uses, in addition to any specific
type printers
"""
from __future__ import annotations

import abc
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

if TYPE_CHECKING:
    # tc only imports
    pass

from acab import AcabConfig
from acab.interfaces.value import ValueFactory as VF

config = AcabConfig()

# Print signals with no semantic component are Str's
TYPE_INSTANCE      = config.attr.Value.Structure.TYPE_INSTANCE
MODAL              = config.attr.Print.Signals.MODAL
SYMBOL             = config.attr.Print.Signals.SYMBOL
ANNOTATIONS        = config.attr.Print.Signals.ANNOTATIONS
ANNOTATIONS_FINAL  = config.attr.Print.Signals.ANNOTATIONS_FINAL
CONSTRAINT         = config.attr.Print.Signals.CONSTRAINT
IMPLICIT_CONTAINER = config.attr.Print.Signals.IMPLICIT_CONTAINER
TAGS               = config.attr.Print.Signals.TAGS

# Signals with semantic/type relevance are Sentences
INSTR              = VF.sen() << config.attr.Type.Primitive.INSTRUCT
CONTAINER          = INSTR << config.attr.Type.Primitive.CONTAINER
STRUCTURE          = INSTR << config.attr.Type.Primitive.STRUCTURE

SENTENCE           = VF.sen() << config.attr.Type.Primitive.SENTENCE
COMPONENT          = SENTENCE << config.attr.Type.Primitive.COMPONENT

ATOM               = VF.sen() << config.attr.Data.TYPE_BASE
