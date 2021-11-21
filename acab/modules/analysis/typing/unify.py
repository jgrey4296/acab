#!/usr/bin/env python3
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from uuid import uuid1

import pyparsing as pp
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.data.values import AcabValue, Sentence
from acab.core.parsing.consts import s, s_key
from acab.error.semantic_exception import AcabSemanticException
from acab.modules.context.context_set import (ContextInstance,
                                              MutableContextInstance)

from . import type_exceptions as TE

logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

unify_enum = Enum("Unify Logic Handler Responses", "NEXT_WORD NA END")

@dataclass
class UnifyLogic:
    """
    Component functions for Acab Unification
    """
    entry_transform : Callable[[AT.Sentence, AT.Sentence, AT.CtxIns], Tuple[AT.Sentence, AT.Sentence]]
    sieve           : List[Callable[[AT.Value, AT.Value, AT.CtxIns], unify_enum]]
    early_exit      : Callable[[AT.Sentence, AT.Sentence, AT.CtxIns], unify_enum]
    truncate        : Callable[[AT.Sentence, AT.Sentence], Tuple[AT.Sentence, AT.Sentence]]



    apply           : Callable[[AT.Sentence, AT.CtxIns], AT.Sentence]

def unify_sentence_pair(first: AT.Sentence,
                        second: AT.Sentence,
                        ctx:AT.CtxIns,
                        logic: UnifyLogic) -> AT.CtxIns:
    """
    """
    # Gamma'
    ctx_prime = ctx
    if not isinstance(ctx_prime, MutableContextInstance):
        ctx_prime = MutableContextInstance(None, ctx)

    with ctx_prime:
        if logic.early_exit is not None and logic.early_exit(first, second, ctx_prime) is unify_enum.END:
            return ctx_prime.finish()

        if logic.truncate is not None:
            first, second = logic.truncate(first, second)

        if logic.entry_transform is not None:
            first, second, _ = logic.entry_transform(first, second, ctx_prime)

        if len(first) != len(second):
            raise TE.AcabMiscTypingException("Unification length mismatch: {len(first)}, {len(second)}")

        for f_word,s_word in zip(first, second):
            result = None
            for sieve_fn in logic.sieve:
                result = sieve_fn(f_word, s_word, ctx_prime)
                if result is unify_enum.NA:
                    continue
                elif result is unify_enum.NEXT_WORD:
                    break
                elif result is unify_enum.END:
                    break

            if result is unify_enum.END:
                break

    return ctx_prime.finish()




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
