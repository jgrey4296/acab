#!/usr/bin/env python3
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab import AcabConfig
from acab import types as AT
from acab.interfaces import value as VI
from acab.core.value.instruction import Instruction
from acab.error.semantic import AcabSemanticException
from acab.modules.context.context_instance import (ContextInstance,
                                                   MutableContextInstance)

from .. import exceptions  as TE
from . import util
from . import unifier

config     = AcabConfig()
unify_enum = util.unify_enum


# TODO handlers for params, tags
# TODO to_word handling

# Basic Unification ###########################################################
def var_handler_basic(index, first, second, ctx, unifier=None):
    """ Bind vars, preferring Ctx -> L -> R
    ctx: f(A) -> set[A]
    """
    result  = unify_enum.NA
    f_word  = first[index]
    s_word  = second[index]
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

def check_modality(index, first, second, ctx, unifier=None):
    """
    Test registered modalities between two words.
    Placing *after* var handler means variable modalities too,
    Placing *before* means a sentence has to match
    modalities prior to substitution
    """
    result = unify_enum.NA
    f_word = first[index]
    s_word = second[index]


    for modality in config.enums['MODAL']:
        if (modality.name not in f_word.data or
            modality.name not in s_word.data):
            continue

        f_mod = f_word.data[modality.name]
        s_mod = s_word.data[modality.name]
        if f_mod != s_mod:
            raise TE.AcabUnifyModalityException(f_word,
                                                s_word,
                                                ctx=ctx,
                                                data={"modality" : modality.name})


    return result

def match_handler_basic(index, first, second, ctx, unifier=None):
    result = unify_enum.NA
    f_word  = first[index]
    s_word  = second[index]

    if isinstance(f_word, VI.Sentence_i) and isinstance(s_word, VI.Sentence_i):
        # TODO handle var args in the type constructors,
        # so recursively unify
        unifier(f_word, s_word, ctx)
        result = unify_enum.NEXT_WORD
    elif ctx[f_word] == ctx[s_word]:
        result = unify_enum.NEXT_WORD
    elif ((isinstance(f_word, VI.Sentence_i) and not s_word.is_var)
          or (isinstance(s_word, VI.Sentence_i) and not f_word.is_var)):
        raise TE.TypeConflictException(f_word, s_word, ctx=ctx)
    elif (ctx[f_word] != ctx[s_word]):
        raise TE.TypeConflictException(f_word, s_word, ctx=ctx)

    return result

def fail_handler_basic(index, first, second, ctx, unifier=None):
    raise TE.AcabUnifySieveFailure(first[index], second[index], ctx=ctx)


def apply_substitutions(sen, gamma) -> AT.Sentence:
    """
    Apply basic substitutions to a sentence
    for any word, get its top most variable, and reify it
    """
    return sen.copy(value=[gamma[util.top_var(x, gamma)] for x in sen.words])


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
