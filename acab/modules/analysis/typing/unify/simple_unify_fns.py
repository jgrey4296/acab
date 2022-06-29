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
from acab.interfaces.bind import Bind_i

from .. import exceptions  as TE
from . import util
from . import unifier

config     = AcabConfig()
unify_enum = util.unify_enum

Bind       = config.prepare("Imports.Targeted", "bind", actions=[config.actions_e.IMCLASS], args={"interface": Bind_i})()

# TODO handlers for params, tags
# TODO to_word handling

# Basic Unification ###########################################################
def whole_sentence_bind(first, second, ctx, unifier=None):
    """
    Early exit if all you have is a variable
    """
    result = unify_enum.NA
    match (first.is_var, len(first), second.is_var, len(second)):
        case _, 1, _, 1:
            pass
        case True, 1, _, _:
            assert(first[0] not in ctx)
            result = unify_enum.END
            ctx[first[0].key()] = second
        case _, _, True, 1:
            assert(second[0] not in ctx)
            result = unify_enum.END
            ctx[second[0].key()] = first

    return result

def var_handler(index, first, second, ctx, unifier=None):
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

    if f_canon == s_canon:
        pass
    elif f_var and f_word not in ctx:
        assert(f_word == f_canon)
        # bind f_word -> s_word
        # result      = unify_enum.NEXT_WORD
        ctx[f_word] = s_canon
    elif s_var and s_word not in ctx:
        assert(s_word == s_canon)
        # bind s_word -> f_word
        # result      = unify_enum.NEXT_WORD
        ctx[s_word] = f_canon

    return result

def equality_check(index, first, second, ctx, unifier=None):
    result = unify_enum.NA
    f_word = first[index]
    s_word = second[index]
    f_var = f_word.is_var
    s_var = s_word.is_var

    if isinstance(f_word, VI.Sentence_i) or isinstance(s_word, VI.Sentence_i):
        return result
    elif not (f_var or s_var) and f_word != s_word:
        raise TE.AcabTypingException(f_word, s_word, ctx=ctx)

    f_canon = util.top_var(f_word, ctx)
    s_canon = util.top_var(s_word, ctx)
    if (f_var or s_var) and ctx[f_canon] != ctx[s_canon]:
        raise TE.AcabUnifyVariableInconsistencyException(f_word, s_word, ctx=ctx)


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

def sentence_recursion(index, first, second, ctx, unifier=None):
    result = unify_enum.NA
    f_word  = first[index]
    s_word  = second[index]

    if isinstance(f_word, VI.Sentence_i) and isinstance(s_word, VI.Sentence_i):
        # TODO handle var args in the type constructors,
        # recursively unify
        unifier(f_word, s_word, ctx)
        result = unify_enum.NEXT_WORD
    elif ((isinstance(f_word, VI.Sentence_i) and not s_word.is_var)
          or (isinstance(s_word, VI.Sentence_i) and not f_word.is_var)):
        raise TE.TypeConflictException(f_word, s_word, ctx=ctx)

    return result

def fail_handler_basic(index, first, second, ctx, unifier=None):
    raise TE.AcabUnifySieveFailure(first[index], second[index], ctx=ctx)

def next_word(index, first, second, ctx, unifier=None):
    return unify_enum.NEXT_WORD

def apply_substitutions(sen, ctx) -> AT.Sentence:
    """
    Apply basic substitutions to a sentence
    for any word, get its top most variable, and reify it
    """
    words = []
    return Bind.bind(sen, ctx)
    for word in sen:
        top_var = util.top_var(word, ctx)
        bound   = Bind.bind(top_var, ctx)
        if isinstance(bound, VI.Sentence_i):
            words += bound
        else:
            words.append(bound)


    return sen.copy(value=words)


#  ############################################################################
basic_sen_logic = unifier.UnifyLogic(
    entry_transform=None,
    early_exit=whole_sentence_bind,
    truncate=util.sen_truncate,
    sieve=[ # Variable Handling
        var_handler,
        # Value Handling
        equality_check,
        check_modality,
        sentence_recursion,
        # Continue
        next_word
        ],
    apply=apply_substitutions
    )

basic_unify = unifier.Unifier(basic_sen_logic)
