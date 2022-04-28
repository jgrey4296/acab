#!/usr/bin/env python3
import logging as logmod
import sys
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

from acab import types as AT
from acab.core.value.default_structure import TYPE_INSTANCE
from acab.core.value import default_structure as DS
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.core.value.value import AcabValue
from acab.core.util.decorators.util import factory
from acab.error.semantic import AcabSemanticException
from acab.modules.context.context_instance import (ContextInstance,
                                                   MutableContextInstance)
from acab.core.parsing.annotation import ValueAnnotation

from .. import exceptions as TE
from . import simple_unify_fns as suf
from . import unifier, util
from .util import INFINITY, type_len, unify_enum

ATOM = "_:{}".format(DS.TYPE_BASE)

def most_gen_type(*args):
    most_gen = (type_len(args[0]), args[0])
    for x in args[1:]:
        new_len = type_len(x)
        if new_len < most_gen[0]:
            most_gen = (new_len, x)

    return most_gen[1]


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
                    gamma_p[var] = Sentence([DS.TYPE_BASE])

    return gamma_p.final_ctx



def var_handler_basic(index, first, second, ctx):
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
    # TODO prefer greater type, even when its on a var
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
def skip_atom_types(index, first, second, ctx):
    """
    If both things you are comparing are atoms, no need to continue the sieve
    """
    result = unify_enum.NA
    f_word  = first[index]
    s_word  = second[index]

    if f_word.is_var or s_word.is_var:
        return result

    # TODO these might need to be reified
    if f_word.type == ATOM and s_word.type == ATOM:
        result = unify_enum.NEXT_WORD

    return result


def test_word_equality(index, first, second, ctx):
    """
    Words have to equal each other, or be a variable
    """
    result = unify_enum.NA
    f_word  = first[index]
    s_word  = second[index]

    has_var = f_word.is_var or s_word.is_var
    if not has_var and f_word != s_word:
        raise TE.TypeConflictException(f_word, s_word, ctx=ctx)

    return result

def match_atom(index, first, second, ctx):
    result = unify_enum.NA
    if second[index] == ATOM:
        result = unify_enum.NEXT_WORD

    return result


def whole_sentence_bind(first, second, ctx):
    """
    Early exit if all you have is a variable
    """
    result = unify_enum.NA
    if first.is_var and ctx[first] == ATOM:
        result = unify_enum.END
    elif not first.is_var and util.top_var(first, ctx) == ATOM:
        result = unify_enum.END
    elif second.is_var and (ctx[second] == ATOM):
        result = unify_enum.END
    elif not second.is_var and util.top_var(second, ctx) == ATOM:
        result = unify_enum.END
    elif not first.is_var and first.type == ATOM and not second.is_var and second.type == ATOM:
        result = unify_enum.END

    return result


def fail_handler_type(index, first, second, ctx):
    raise TE.AcabUnifySieveFailure(first[index], second[index], ctx=ctx)


# Factories ###################################################################
@factory
def var_consistency_check(logic, index, first, second, ctx):
    """
    Check variables stay consistent as you progress through the sentence
    Check words types are consistent with the context
    """
    if not (first[index].is_var or second[index].is_var):
        return unify_enum.NA
    sub_unifier = unifier.Unifier(logic)
    try:
        logging.debug("Checking Vars on: {}, {}", first[index], second[index])
        for sen in [first, second]:
            word = sen[index]
            if not word.is_var:
                continue

            canon_v   = util.top_var(word, ctx)
            canon_w   = ctx[canon_v]
            canon_w_t = util.top_var(canon_w.type, ctx)
            target    = canon_w.type[0] if canon_w.type.is_var else str(sen[:index+1])

            if canon_w_t == word.type:
                continue

            sub_unifier(word.type, canon_w_t, ctx, logic)
            update_typing = min((type_len(canon_w_t), canon_w_t),
                                (type_len(word.type), word.type))[1]

            # logging.debug("Assigning updated typing")
            # ctx[target]   = update_typing
            # if word.type.is_var:
            #     ctx[word.type] = update_typing

    except TE.AcabTypingException as err:
        raise TE.AcabUnifyVariableInconsistencyException(err.left, err.right, ctx=err.ctx) from err

    return unify_enum.NA



@factory
def unify_type_sens(logic, index, first, second, ctx):
    # TODO this can be simplified
    logging.debug("Unifying Type Annotations for: {}, {}", first[index], second[index])
    result = unify_enum.NA
    canon_fv  = util.top_var(first[index], ctx)
    canon_f_t = util.top_var(canon_fv.type, ctx)
    canon_f   = ctx[canon_fv]
    if isinstance(canon_f, ValueAnnotation):
        canon_f = canon_f.value.type
    else:
        canon_f = canon_f.type

    canon_sv  = util.top_var(second[index], ctx)
    canon_s_t = util.top_var(canon_sv.type, ctx)
    canon_s   = ctx[canon_sv]
    if isinstance(canon_s, ValueAnnotation):
        canon_s = canon_s.value.type
    else:
        canon_s = canon_s.type


    # if canon_f_t == canon_s_t:
    #     return unify_enum.NEXT_WORD

    sub_unifier = unifier.Unifier(logic)
    try:
        # Discard the returned context:
        sub_unifier(canon_f_t, canon_s_t, ctx, logic)
    except TE.AcabTypingException as err:
        data = {}
        data.update(err.data)
        data.update({'types': [canon_f_t, canon_s_t]})
        raise TE.TypeConflictException(first[index], second[index], ctx=err.ctx, data=data) from err

    logging.debug("Passed unification, binding general over specific")
    # Update the var in ctx, or update a tight binding of the object
    f_key = canon_f_t if canon_f_t.is_var else str(first[:index+1])
    s_key = canon_s_t if canon_s_t.is_var else str(first[:index+1])

    # Bind the more general type over the more specific type
    update_typing = min((type_len(canon_f_t), s_key, canon_f_t),
                        (type_len(canon_s_t), f_key, canon_s_t))

    current_ctx_type = most_gen_type(canon_f, canon_s)
    ctx_more_gen = update_typing[0] > type_len(current_ctx_type)
    if isinstance(update_typing[1], str):
        current_ctx_type = ValueAnnotation(DS.TYPE_INSTANCE, current_ctx_type)

    if ctx_more_gen:
        ctx[update_typing[1]] = current_ctx_type
    elif not(isinstance(update_typing[1], str)):
        assert(update_typing[1].is_var)
        ctx[update_typing[1]] = update_typing[2]
    else:
        ctx[update_typing[1]] = ValueAnnotation(DS.TYPE_INSTANCE, update_typing[2])
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
        canon_word = gamma[util.top_var(word, gamma)]
        if canon_word == ATOM:
            values.append(word)
        else:
            values.append(canon_word)

    return sen.copy(value=values)



def apply_typed_sen_sub(sen, gamma) -> AT.Sentence:
    """
    Apply Substitutions to sentences and each word's type sentence
    ie: a.test.sentence(::$x), {$x: ::a.type} == a.test.sentence(::a.type)
    """
    # TODO rework this
    words = []
    for index in range(len(sen)):
        word = sen[index]
        if word.is_var:
            canon_var    = util.top_var(word, gamma)
            canon_word   = gamma[canon_var]
            canon_type   = most_gen_type(canon_var.type, canon_word.type)

            if canon_type.has_var:
                canon_type = apply_sen_type_sub(canon_type, gamma)

            words.append(canon_word.copy(data={TYPE_INSTANCE : canon_type}))

        elif str(sen[:index+1]) in gamma:
            binding = gamma[str(sen[:index+1])]
            assert(isinstance(binding, ValueAnnotation))
            updated      = binding(word.copy())
            canon_type   = most_gen_type(word.type, updated.type)
            if canon_type.has_var:
                canon_type = apply_sen_type_sub(canon_type, gamma)
            words.append(updated.copy(data={TYPE_INSTANCE: canon_type}))
        else:
            words.append(word)


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
