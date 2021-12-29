#!/usr/bin/env python3
from dataclasses import InitVar, dataclass, field
from enum import Enum
import sys
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.modules.analysis.typing.simple_unify_fns as suf
from acab import types as AT
from acab.core.data.value import AcabValue, Sentence
from acab.core.decorators.util import factory
from acab.error.semantic import AcabSemanticException
from acab.modules.analysis.typing import unify
from acab.modules.analysis.typing.unify import unify_enum
from acab.modules.context.context_set import (ContextInstance,
                                              MutableContextInstance)

from . import exceptions as TE

INFINITY = sys.maxsize.real

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

def type_len(sen):
    """ Utility to compare lengths of type sentences.
    Sets _:ATOM -> ∞
    """
    if sen.is_var:
        return INFINITY
    if sen == "_:ATOM":
        return INFINITY - 1
    else:
        return len(sen)

def gen_type_vars(first, second, gamma, gen_var=None) -> AT.CtxIns:
    """
    Detect values destined to change,
    and variables,
    add them to the context

    Creating Canonical Variables (id(X)),
    and Canonical Value/Variable types (id(X.type))
    where X is the first instance to be discovered
    """

    if gen_var is None:
        global gen_f
        gen_var = gen_f


    if len(first) != len(second):
        raise TE.AcabMiscTypingException(f"Generating types on length mismatch: {len(first)}, {len(second)}")

    gamma_p = gamma
    if not isinstance(gamma_p, MutableContextInstance):
        gamma_p = MutableContextInstance(None, gamma)

    path_f = []
    path_s = []

    with gamma_p:
        for a,b in zip(first, second):
            path_f.append(a)
            path_f.append(b)
            if not (a.is_var or b.is_var or type_len(b.type) < INFINITY or type_len(a.type) < INFINITY):
                continue

            # Create canonical references
            if a.is_var and a not in gamma_p:
                gamma_p[a]          = a
            if b.is_var and b not in gamma_p:
                gamma_p[b]          = b

            if a.type.is_var and a.type[0] not in gamma_p:
                gamma_p[a.type[0]] = Sentence.build(["ATOM"])

            if b.type.is_var and b.type[0] not in gamma_p:
                gamma_p[b.type[0]] = Sentence.build(["ATOM"])

    return gamma_p.finish()



# Type Unification ############################################################
def match_atom(f_word, s_word, ctx):
    result = unify_enum.NA
    if s_word == "_:ATOM":
        result = unify_enum.NEXT_WORD

    return result

def skip_atom_types(f_word, s_word, ctx):
    result = unify_enum.NA
    if f_word.is_var or s_word.is_var:
        return result

    if f_word.type == "_:ATOM" and s_word.type == "_:ATOM":
        result = unify_enum.NEXT_WORD

    return result


def whole_sentence_bind(first, second, ctx):
    result = unify_enum.NA
    if first.is_var and ctx[first[0]] == "_:ATOM":
        ctx[first[0]] = second
        result = unify_enum.END
    elif not first.is_var and unify.reify(first, ctx) == "_:ATOM":
        ctx[id(first)] = second
        result = unify_enum.END
    elif second.is_var and (ctx[second[0]] == "_:ATOM"):
        ctx[second[0]] = first
        result = unify_enum.END
    elif not second.is_var and unify.reify(second, ctx) == "_:ATOM":
        ctx[id(second)] = first
        result = unify_enum.END
    elif not first.is_var and first.type == "_:ATOM" and not second.is_var and second.type == "_:ATOM":
        result = unify_enum.END

    return result


def fail_handler_type(f_word, s_word, ctx):
    raise TE.TypeConflictException( f_word, s_word, None, ctx)


# Factories ###################################################################
@factory
def unify_type_sens(logic, f_word, s_word, ctx):
    result = unify_enum.NA

    canon_f   = unify.reify(f_word, ctx)
    canon_s   = unify.reify(s_word, ctx)
    canon_f_t = unify.reify(canon_f.type, ctx)
    canon_s_t = unify.reify(canon_s.type, ctx)

    unify.unify_sentence_pair(canon_f_t, canon_s_t, ctx, logic)


    f_key = canon_f_t
    s_key = canon_s_t
    if not f_key.is_var:
        f_key = id(f_key)
    else:
        f_key = f_key[0]
    if not s_key.is_var:
        s_key = id(s_key)
    else:
        s_key = s_key[0]

    if type_len(canon_f_t) < type_len(canon_s_t):
        ctx[s_key] = canon_f_t
    else:
        ctx[f_key] = canon_s_t

    result = unify_enum.NEXT_WORD

    return result


@factory
def var_consistency_check(logic, first, second, ctx):
    if not (first.is_var or second.is_var):
        return unify_enum.NA

    try:
        if first.is_var:
            canon_f   = unify.reify(first, ctx)
            canon_f_t = canon_f.type
            if canon_f.type.is_var and canon_f.type[0] in ctx:
                canon_f_t = unify.reify(canon_f.type[0], ctx)
            elif id(canon_f.type) in ctx:
                canon_f_t = unify.reify(id(canon_f.type), ctx)

            unify.unify_sentence_pair(first.type, canon_f_t, ctx, logic)
            update_typing = min((type_len(canon_f_t), canon_f_t),
                                (type_len(first.type), first.type))[1]
            if canon_f.type.is_var:
                ctx[canon_f.type[0]] = update_typing
            else:
                ctx[id(canon_f_t)]   = update_typing

        if second.is_var:
            canon_s   = unify.reify(second, ctx)
            canon_s_t = canon_s.type
            if canon_s.type.is_var and canon_s.type[0] in ctx:
                canon_s_t = unify.reify(canon_s.type[0], ctx)
            elif id(canon_s.type) in ctx:
                canon_s_t = unify.reify(id(canon_s.type), ctx)

            unify.unify_sentence_pair(second.type, canon_s_t, ctx, logic)
            update_typing = min((type_len(canon_s_t), canon_s_t),
                                (type_len(second.type), second.type))[1]
            if canon_s.type.is_var:
                ctx[canon_s.type[0]] = update_typing
            else:
                ctx[id(canon_s_t)]   = update_typing

    except TE.TypeUnifyException as err:
        raise TE.TypeConflictException(first, second, None, ctx) from err

    return unify_enum.NA




#  ############################################################################
# ::a.b.c
def apply_types_sub(sen, gamma) -> AT.Sentence:
    """
    Apply Substitutions to type sentences of words
    """
    words = []
    for word in sen.words:
        canon_word   = unify.reify(word, gamma)
        canon_word_t = unify.reify(canon_word.type, gamma)
        if canon_word_t.has_var:
            canon_word_t = suf.apply_substitutions(canon_word_t, gamma)

        words.append(canon_word.copy(data={'TYPE_INSTANCE' : canon_word_t}))

    return sen.copy(value=words)


type_as_sen_logic = unify.UnifyLogic(
    entry_transform=None,
    early_exit=whole_sentence_bind,
    truncate=unify.sen_extend,
    sieve=[match_atom,
           suf.var_handler_basic,
           suf.match_handler_basic,
           suf.fail_handler_basic],
    apply=suf.apply_substitutions)

type_sen_unify = lambda x, y, c: unify.unify_sentence_pair(x, y, c, type_as_sen_logic)

# a.b.c(::d.e.f)
typed_sen_logic = unify.UnifyLogic(
    entry_transform=lambda x,y,c: (x, y, gen_type_vars(x, y, c)),
    early_exit=None,
    truncate=unify.sen_extend,
    sieve=[skip_atom_types,
           var_consistency_check(type_as_sen_logic),
           unify_type_sens(type_as_sen_logic),
           fail_handler_type],
    apply=apply_types_sub
    )

type_unify = lambda x, y, c: unify.unify_sentence_pair(x, y, c, typed_sen_logic)
