#!/usr/bin/env python3
import logging as root_logger
import sys
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
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

def top_var(val, gamma):
    last = None
    current = val
    while current.is_var and last != val:
        last = current
        if isinstance(current, Sentence) and current[0] in gamma:
            current = gamma[current[0]]
        else:
            current = gamma[current]

    return last or current

def reify(val, gamma):
    last = None
    current = val
    while current != last:
        last = current
        if id(current) in gamma:
            current = gamma[id(current)]
        elif isinstance(current, Sentence) and current.is_var and current[0] in gamma:
            current = gamma[current[0]]
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
