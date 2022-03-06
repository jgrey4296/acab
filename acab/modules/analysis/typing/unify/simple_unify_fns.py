#!/usr/bin/env python3
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab import GET
from acab import types as AT
from acab.core.data.value import AcabValue, Sentence
from acab.error.semantic import AcabSemanticException
from acab.modules.context.context_set import (ContextInstance,
                                              MutableContextInstance)

from .. import exceptions  as TE
from . import util
from . import unifier

config = GET()
unify_enum = util.unify_enum


# TODO handlers for params, tags
# TODO to_word handling

# Basic Unification ###########################################################
def var_handler_basic(f_word, s_word, ctx):
    """ Bind vars, preferring Ctx -> L -> R
    ctx: f(A) -> set[A]
    """
    result  = unify_enum.NA
    f_var   = f_word.is_var
    s_var   = s_word.is_var

    if not (f_var or s_var):
        return result

    f_canon = util.top_var(f_word, ctx)
    s_canon = util.top_var(s_word, ctx)

    if f_var and f_word not in ctx:
        # bind f_word -> s_word
        result      = unify_enum.NEXT_WORD
        ctx[f_word] = s_canon
    elif s_var and s_word not in ctx:
        # bind s_word -> f_word
        result      = unify_enum.NEXT_WORD
        ctx[s_word] = f_canon
    elif f_var and f_word in ctx and ctx[f_word] == f_word:
        ctx[f_word] = s_canon
    elif s_var and s_word in ctx and ctx[s_word] == s_word:
        ctx[s_word] = f_canon


    return result

def check_modality(f_word, s_word, ctx):
    """
    Test registered modalities between two words.
    Placing *after* var handler means variable modalities too,
    Placing *before* means a sentence has to match
    modalities prior to substitution
    """
    result = unify_enum.NA

    for modality in config.enums['MODAL']:
        if modality.name not in ctx[f_word].data or modality.name not in ctx[s_word].data:
            continue

        f_mod = ctx[f_word].data[modality.name]
        s_mod = ctx[s_word].data[modality.name]
        if f_mod != s_mod:
            raise TE.AcabUnifyModalityException(f_word,
                                                s_word,
                                                ctx=ctx,
                                                data={"modality name" : modality.name})


    return result

def match_handler_basic(f_word, s_word, ctx):
    result = unify_enum.NA
    if ctx[f_word] == ctx[s_word]:
        result = unify_enum.NEXT_WORD
    elif isinstance(f_word, Sentence) or isinstance(s_word, Sentence):
        # TODO handle var args in the type constructors,
        # so recursively unify
        raise NotImplementedError()

    return result

def fail_handler_basic(f_word, s_word, ctx):
    raise TE.AcabUnifySieveFailure(f_word, s_word, ctx=ctx)


def apply_substitutions(sen, gamma) -> AT.Sentence:
    """
    Apply basic substitutions to a sentence
    """
    return sen.copy(value=[util.reify(x, gamma) for x in sen.words])


#  ############################################################################
basic_sen_logic = unifier.UnifyLogic(
    entry_transform=None,
    early_exit=None,
    truncate=util.sen_truncate,
    sieve=[var_handler_basic,
           check_modality,
           match_handler_basic,
           fail_handler_basic],
    apply=apply_substitutions
    )

basic_unify = unifier.Unifier(basic_sen_logic)
