#!/usr/bin/env python3
import sys
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab import types as AT
from acab.core.data.value import AcabValue
from acab.core.data.instruction import Instruction
from acab.core.data.sentence import Sentence
from acab.core.decorators.util import factory
from acab.error.semantic import AcabSemanticException
from acab.modules.context.context_set import (ContextInstance,
                                              MutableContextInstance)
from acab.core.data.default_structure import TYPE_INSTANCE

from . import simple_unify_fns as suf
from .. import exceptions as TE
from . import unifier, util
from .util import unify_enum, type_len, INFINITY

def gen_type_vars(first, second, gamma, gen_var=None) -> AT.CtxIns:
    """
    Detect values and variables destined to change,
    add them to the context
    """

    if gen_var is None:
        gen_var = util.gen_f

    if len(first) != len(second):
        raise TE.AcabLengthUnifyException(first, second, ctx=gamma)

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

            for var in (a.type.vars + b.type.vars):
                if var not in gamma_p:
                    gamma_p[var] = Sentence(["ATOM"])

    return gamma_p.finish()



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
    # TODO prefer greater type, even when its on a var
    #
    if f_var and f_word not in ctx:
        # bind f_word -> s_word
        ctx[f_word] = s_canon
    elif s_var and s_word not in ctx:
        # bind s_word -> f_word
        ctx[s_word] = f_canon
    elif f_var and ctx[f_word] == f_word:
        ctx[f_word] = s_canon
    elif s_var and ctx[s_word] == s_word:
        ctx[s_word] = f_canon


    return result


# Type Unification ############################################################
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


def test_word_equality(f_word, s_word, ctx):
    """
    Words have to equal each other, or be a variable
    """
    result = unify_enum.NA
    has_var = f_word.is_var or s_word.is_var
    if not has_var and f_word != s_word:
        raise TE.TypeConflictException(f_word, s_word, ctx=ctx)

    return result

def match_atom(f_word, s_word, ctx):
    result = unify_enum.NA
    if s_word == "_:ATOM":
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
    raise TE.AcabUnifySieveFailure(f_word, s_word, ctx=ctx)


# Factories ###################################################################
@factory
def var_consistency_check(logic, first, second, ctx):
    """
    Check words types are consistent with the context
    """
    if not (first.is_var or second.is_var):
        return unify_enum.NA
    sub_unifier = unifier.Unifier(logic)
    try:
        for word in [first, second]:
            if not word.is_var:
                continue

            canon_w   = util.reify(word, ctx)
            canon_w_t = util.reify(canon_w.type, ctx)
            target    = canon_w.type[0] if canon_w.type.is_var else id(canon_w_t)

            if canon_w_t == word.type:
                continue

            sub_unifier(word.type, canon_w_t, ctx, logic)
            update_typing = min((type_len(canon_w_t), canon_w_t),
                                (type_len(word.type), word.type))[1]
            ctx[target]   = update_typing
            if word.type.is_var:
                ctx[word.type] = update_typing

    except TE.AcabTypingException as err:
        raise TE.AcabUnifyVariableInconsistencyException(err.left, err.right, ctx=err.ctx) from err

    return unify_enum.NA



@factory
def unify_type_sens(logic, f_word, s_word, ctx):
    result = unify_enum.NA
    canon_f   = util.reify(f_word, ctx)
    canon_f_t = util.reify(canon_f.type, ctx)

    canon_s   = util.reify(s_word, ctx)
    canon_s_t = util.reify(canon_s.type, ctx)

    # if canon_f_t == canon_s_t:
    #     return unify_enum.NEXT_WORD

    sub_unifier = unifier.Unifier(logic)
    # Discard the returned context:
    try:
        sub_unifier(canon_f_t, canon_s_t, ctx, logic)
    except TE.AcabTypingException as err:
        data = {}
        data.update(err.data)
        data.update({'types': [canon_f_t, canon_s_t]})
        raise TE.TypeConflictException(f_word, s_word, ctx=err.ctx, data=data) from err


    # Update the var in ctx, or update a tight binding of the object
    f_key = canon_f_t if canon_f_t.is_var else id(canon_f_t)
    s_key = canon_s_t if canon_s_t.is_var else id(canon_s_t)

    # Bind the more general type over the more specific type
    update_typing = min((type_len(canon_f_t), s_key, canon_f_t),
                        (type_len(canon_s_t), f_key, canon_s_t))

    ctx[update_typing[1]] = update_typing[2]
    result = unify_enum.NEXT_WORD

    return result



#  ############################################################################
# ::a.b.c
def apply_sen_type_sub(sen, gamma) -> AT.Sentence:
    """
    Apply substitutions to a type sentence
    The important logic here is to skip any variable which reduces to ATOM
    ie: ::a.type.sen.$x, {$x: ATOM} == ::a.type.sen.$x
    """
    values = []
    for word in sen.words:
        canon_word = util.reify(word, gamma)
        if canon_word == "_:ATOM":
            values.append(word)
        else:
            values.append(canon_word)

    return sen.copy(value=values)



def apply_typed_sen_sub(sen, gamma) -> AT.Sentence:
    """
    Apply Substitutions to sentences and each word's type sentence
    ie: a.test.sentence(::$x), {$x: ::a.type} == a.test.sentence(::a.type)
    """
    words = []
    for word in sen.words:
        canon_word   = util.reify(word, gamma)
        canon_word_t = util.reify(canon_word.type, gamma)
        if canon_word_t.has_var:
            canon_word_t = apply_sen_type_sub(canon_word_t, gamma)

        words.append(canon_word.copy(data={TYPE_INSTANCE : canon_word_t}))

    return sen.copy(value=words)


type_as_sen_logic = unifier.UnifyLogic(
    entry_transform=None,
    early_exit=whole_sentence_bind,
    truncate=util.sen_extend,
    sieve=[match_atom,
           suf.var_handler_basic,
           suf.check_modality,
           suf.match_handler_basic,
           suf.fail_handler_basic],
    apply=apply_sen_type_sub)

type_sen_unify = unifier.Unifier(type_as_sen_logic)

# a.b.c(::d.e.f)
typed_sen_logic = unifier.UnifyLogic(
    entry_transform=lambda x,y,c: (x, y, gen_type_vars(x, y, c)),
    early_exit=None,
    truncate=util.sen_extend,
    sieve=[var_handler_basic,
           suf.check_modality,
           test_word_equality,
           skip_atom_types,
           var_consistency_check(type_as_sen_logic),
           unify_type_sens(type_as_sen_logic),
           fail_handler_type],
    apply=apply_typed_sen_sub
    )

type_unify = unifier.Unifier(typed_sen_logic)
