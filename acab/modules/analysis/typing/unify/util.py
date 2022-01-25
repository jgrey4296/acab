#!/usr/bin/env python3
import logging as root_logger
import sys
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast, TypeAlias)
from uuid import uuid1

import pyparsing as pp
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.data.value import AcabValue, Sentence
from acab.core.parsing.consts import s, s_key
from acab.error.semantic import AcabSemanticException
from acab.modules.context.context_set import (ContextInstance,
                                              MutableContextInstance)

from .. import exceptions as TE

logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

Value  : TypeAlias = AT.Value
CtxIns : TypeAlias = AT.CtxIns

unify_enum = Enum("Unify Logic Handler Responses", "NEXT_WORD NA END")

def gen_var() -> Callable[[], AT.Value]:
    """ A Simple Generator of guaranteed new Variables """
    counter = 0
    def wrapped() -> AT.Value:
        nonlocal counter
        new_name = "GenVar_{}".format(counter)
        counter += 1
        return Sentence.build([AcabValue.safe_make(new_name, data={"BIND":True})])

    return wrapped



gen_f  = gen_var()

# Sentence Length Handling ####################################################
def sen_extend(first, second):
    f_words = first.words
    s_words = second.words
    if len(first) > len(second):
        s_words += first[len(second):].words
    else:
        f_words += second[len(first):].words

    return first.copy(value=f_words), second.copy(value=s_words)

def sen_truncate(first, second):
    f_words = first.words
    s_words = second.words
    if len(first) > len(second):
        f_words = f_words[:len(second)]
    else:
        s_words = s_words[:len(first)]

    return first.copy(value=f_words), second.copy(value=s_words)

def top_var(val:Value, gamma:CtxIns) -> Value:
    """
    Given a value, return it if not a variable,
    otherwise repetaedly look it up in gamma
    and get the last variable before it turns into a proper value

    eg: $x, {$x: $y, $y: $z, $z: hello}
    -> $z
    """
    last = None
    current = val
    while current.is_var and last != val:
        last    = current
        current = gamma[current]

    return last or current

def reify(val:Value, gamma:CtxIns) -> Value:
    """
    Given a value, look it up in gamma
    eg: $x, {$x: $y, $y: $z, $z: hello}
    -> hello

    """
    last = None
    current = val
    while current != last:
        last = current
        if id(current) in gamma:
            current = gamma[id(current)]
        else:
            current = gamma[current]

    return current


# Type Functions ##############################################################
INFINITY = sys.maxsize.real

def type_len(sen):
    """ Utility to compare lengths of type sentences.
    _:ATOM == ∞ - 1
    $x == ∞
    """
    if sen.is_var:
        return INFINITY
    if sen == "_:ATOM":
        return INFINITY - 1
    else:
        return len(sen)
