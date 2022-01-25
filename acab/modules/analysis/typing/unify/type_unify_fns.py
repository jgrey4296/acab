#!/usr/bin/env python3
import sys
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab import types as AT
from acab.core.data.value import AcabValue, Sentence
from acab.core.decorators.util import factory
from acab.error.semantic import AcabSemanticException
from acab.modules.context.context_set import (ContextInstance,
                                              MutableContextInstance)

from . import simple_unify_fns as suf
from .. import exceptions as TE
from . import unify, util
from .util import unify_enum, type_len, INFINITY

def gen_type_vars(first, second, gamma, gen_var=None) -> AT.CtxIns:
    """
    Detect values and variables destined to change,
    add them to the context
    """

    if gen_var is None:
        gen_var = util.gen_f

    if len(first) != len(second):
        raise TE.AcabMiscTypingException(f"Generating types on length mismatch: {len(first)}, {len(second)}")

    gamma_p = MutableContextInstance(None, gamma)

    with gamma_p:
        for a,b in zip(first, second):
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
    """
    If both things you are comparing are atoms, no need to continue the sieve
    """
    result = unify_enum.NA
    if f_word.is_var or s_word.is_var:
        return result

    # TODO these might need to be reified
    if f_word.type == "_:ATOM" and s_word.type == "_:ATOM":
        result = unify_enum.NEXT_WORD

    return result


def whole_sentence_bind(first, second, ctx):
    """
    Early exit if all you have is a variable
    """
    result = unify_enum.NA
    if first.is_var and ctx[first] == "_:ATOM":
        result = unify_enum.END
    elif not first.is_var and util.reify(first, ctx) == "_:ATOM":
        result = unify_enum.END
    elif second.is_var and (ctx[second] == "_:ATOM"):
        result = unify_enum.END
    elif not second.is_var and util.reify(second, ctx) == "_:ATOM":
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

    canon_f   = util.reify(f_word, ctx)
    canon_f_t = util.reify(canon_f.type, ctx)

    canon_s   = util.reify(s_word, ctx)
    canon_s_t = util.reify(canon_s.type, ctx)

    sub_unifier = unify.Unifier(logic)
    # Discard the returned context:
    sub_unifier(canon_f_t, canon_s_t, ctx, logic)


    # Update the var in ctx, or update a tight binding of the object
    f_key = canon_f_t if canon_f_t.is_var else id(canon_f_t)
    s_key = canon_s_t if canon_s_t.is_var else id(canon_s_t)

    # Bind the more general type over the more specific type
    update_typing = min((type_len(canon_f_t), s_key, canon_f_t),
                        (type_len(canon_s_t), f_key, canon_s_t))

    ctx[update_typing[1]] = update_typing[2]
    result = unify_enum.NEXT_WORD

    return result


@factory
def var_consistency_check(logic, first, second, ctx):
    """
    Check words types are consistent with the context
    """
    if not (first.is_var or second.is_var):
        return unify_enum.NA

    sub_unifier = unify.Unifier(logic)

    try:
        for word in [first, second]:
            if not word.is_var:
                continue

            canon_w   = util.reify(word, ctx)
            canon_w_t = util.reify(canon_w.type, ctx)
            target    = canon_w.type[0] if canon_w.type.is_var else id(canon_w_t)

            if canon_w_t == word.type:
                pass

            sub_unifier(word.type, canon_w_t, ctx, logic)
            update_typing = min((type_len(canon_w_t), canon_w_t),
                                (type_len(word.type), word.type))[1]
            ctx[target]   = update_typing

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
        canon_word   = util.reify(word, gamma)
        canon_word_t = util.reify(canon_word.type, gamma)
        if canon_word_t.has_var:
            canon_word_t = suf.apply_substitutions(canon_word_t, gamma)

        words.append(canon_word.copy(data={'TYPE_INSTANCE' : canon_word_t}))

    return sen.copy(value=words)


type_as_sen_logic = unify.UnifyLogic(
    entry_transform=None,
    early_exit=whole_sentence_bind,
    truncate=util.sen_extend,
    sieve=[match_atom,
           suf.var_handler_basic,
           suf.match_handler_basic,
           suf.fail_handler_basic],
    apply=suf.apply_substitutions)

type_sen_unify = unify.Unifier(type_as_sen_logic)

# a.b.c(::d.e.f)
typed_sen_logic = unify.UnifyLogic(
    entry_transform=lambda x,y,c: (x, y, gen_type_vars(x, y, c)),
    early_exit=None,
    truncate=util.sen_extend,
    sieve=[skip_atom_types,
           var_consistency_check(type_as_sen_logic),
           unify_type_sens(type_as_sen_logic),
           fail_handler_type],
    apply=apply_types_sub
    )

type_unify = unify.Unifier(typed_sen_logic)
