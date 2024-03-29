#!/usr/bin/env python3
import logging as logmod
from re import sub
import sys
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from string import Template

logging = logmod.getLogger(__name__)

import acab.core.defaults.value_keys as DS
from acab import AcabConfig
from acab import types as AT
from acab.core.defaults.value_keys import TYPE_INSTANCE
from acab.core.util.annotation import ValueAnnotation
from acab.core.util.decorators.util import factory
from acab.core.value.instruction import Instruction
from acab.error.semantic import AcabSemanticException
from acab.interfaces import value as VI
from acab.interfaces.bind import Bind_i
from acab.interfaces.context import ContextSet_i
from acab.interfaces.value import ValueFactory as VF
from acab.modules.context.context_instance import (ContextInstance,
                                                   MutableContextInstance)

from .. import exceptions as TE
from . import simple_unify_fns as suf
from . import util
from .unifier import Unifier, UnifyLogic
from .util import INFINITY, type_len, unify_enum

config = AcabConfig()
ContextSet = config.prepare("Imports.Targeted", "context", actions=[config.actions_e.IMCLASS], args={"interface": ContextSet_i})()
Bind       = config.prepare("Imports.Targeted", "bind", actions=[config.actions_e.IMCLASS], args={"interface": Bind_i})()


ATOM = "_:{}".format(DS.TYPE_BASE)

type_t = Template("$base^type")

# Utility ############################################################
def most_gen_type(*args):
    """
    from the args, get the type that is shortest

    eg: ::a.b.c, ::a.b -> a.b

    """
    # most_gen = (type_len(args[0]), args[0])
    # for x in args[1:]:
    #     new_len = type_len(x)
    #     if new_len < most_gen[0]:
    #         most_gen = (new_len, x)
    most_gen = [(type_len(x), x) for x in args]

    return min(most_gen)[1]


def gen_type_vars(first, second, ctx, gen_var=None) -> AT.CtxIns:
    """
    Detect values and variables destined to change,
    add them to the context
    """

    if gen_var is None:
        gen_var = util.gen_f

    if len(first) != len(second):
        raise TE.AcabLengthUnifyException(first, second, ctx=ctx)

    ctx_p = MutableContextInstance(ctx)
    with ctx_p:
        for a,b in zip(first, second):
            if not (a.has_var or b.has_var or type_len(b.type) < INFINITY or type_len(a.type) < INFINITY):
                continue

            # Create canonical references
            match a, a.is_var, a not in ctx_p:
                case VI.Value_i(), True, True:
                    ctx_p[a] = a

            match b, b.is_var, b not in ctx_p:
                case VI.Value_i(), True, True:
                    ctx_p[b.key()] = b

            for var in (a.type.vars + b.type.vars):
                if var not in ctx_p:
                    ctx_p[var] = VF.sen() << DS.TYPE_BASE

    return ctx_p.final_ctx



# Variable Handlers ############################################################
def var_handler(index, first, second, ctx, unifier=None):
    """ Bind vars, preferring Ctx -> L -> R
    ctx: f(A) -> set[A]
    """
    result  = unify_enum.NA
    f_word, s_word = first[index], second[index]
    f_var,  s_var  = f_word.is_var, s_word.is_var

    if not (f_var or s_var):
        return result
    if isinstance(f_word, VI.Sentence_i) or isinstance(s_word, VI.Sentence_i):
        return result

    f_canon, s_canon = ctx[f_word], ctx[s_word]

    if f_canon == s_canon:
        pass
    elif s_var and s_word not in ctx:
        assert(s_word == s_canon)
        # bind s_word -> f_word
        ctx[s_word]           = f_canon
    elif f_var and f_word not in ctx:
        assert(f_word == f_canon)
        # bind f_word -> s_word
        ctx[f_word]           = s_canon

    return result

# Value Handlers ############################################################
def typed_sentence_recursion(index, first, second, ctx, unifier=None):
    result = unify_enum.NA
    f_word, s_word  = first[index], second[index]

    if not (isinstance(f_word, VI.Sentence_i) or isinstance(s_word, VI.Sentence_i)):
        return result

    match f_word, s_word:
        case VI.Sentence_i(), VI.Sentence_i():
            type_unifier = Unifier(unifier.logic.sub_logic)
            # recursively unify using a *separate* copy of the context
            partial   = unifier(f_word, s_word, ctx.copy())
            # And unify the types of the sentences
            completed = type_unifier(f_word.type, s_word.type, partial)
            # then update the context with explicit path annotations like in unifty_type_sens
            ctx.progress({x:y for x,y in completed if x not in ctx and not isinstance(y, ValueAnnotation)}, None)
            for key, value in completed:
                if isinstance(value, ValueAnnotation):
                    prefix = first[:index].key(suffix=key)
                    logging.debug("Prefix for annotation is: {}", first[:index])
                    ctx[str(prefix)] = value

            result = unify_enum.NEXT_WORD
        case VI.Sentence_i(), _ if s_word.is_var and s_word not in ctx:
            ctx[s_word] = f_word
        case VI.Sentence_i(), _  if s_word.is_var and ctx[s_word] != f_word:
            raise TE.AcabUnifyFailure(f_word, s_word, ctx=ctx)
        case _, VI.Sentence_i() if f_word.is_var and f_word not in ctx:
            ctx[f_word] = s_word
        case _, VI.Sentence_i() if f_word.is_var and ctx[f_word] != s_word:
            raise TE.AcabUnifyFailure(f_word, s_word, ctx=ctx)
        case _, _:
            raise TE.TypeConflictException(f_word, s_word, ctx=ctx)

    return result


# Type Unification ############################################################
def atom_types_satisifed(index, first, second, ctx, unifier=None):
    """
    If both things you are comparing are atoms,
    no need to continue the sieve
    """
    result = unify_enum.NA
    f_word, s_word = first[index], second[index]
    f_var,  s_var  = f_word.is_var, s_word.is_var

    if f_word.is_var or s_word.is_var:
        return result

    if f_word.type == ATOM and s_word.type == ATOM:
        result = unify_enum.NEXT_WORD

    return result


def fix_types(index, first, second, ctx, unifier=None):
    result  = unify_enum.NA
    f_word, s_word = first[index], second[index]
    f_var,  s_var  = f_word.is_var, s_word.is_var

    if f_var:
        f_type_key = type_t.substitute(base=f_word.key())
        f_t_source = f_word[0].type if isinstance(f_word, VI.Sentence_i) else f_word.type
        f_type     = ctx[f_t_source] if f_t_source.is_var else f_t_source
        if f_type_key not in ctx:
            ctx[f_type_key] = f_type
        elif ctx[f_type_key] != ATOM:
            pass

    if s_var:
        s_type_key = type_t.substitute(base=s_word.key())
        s_t_source = s_word[0].type if isinstance(s_word, VI.Sentence_i) else s_word.type
        s_type     = ctx[s_t_source] if s_t_source.is_var else s_t_source
        if s_type_key not in ctx:
            ctx[s_type_key] = s_type
        elif ctx[s_type_key] != ATOM:
            pass

    # if s_var:
    #     s_type_key = type_t.substitute(base=s_word.key())
    #     if s_type_key not in ctx:
    #         ctx[s_type_key] = s_word.type
    #     elif ctx[s_type_key] != ATOM:
    #         pass
    #     elif not ctx[s_word].is_var and ctx[s_word].type.is_var:
    #         ctx[s_type_key] = ctx[s_word].type[0]
    #     elif not ctx[s_word].is_var:
    #         ctx[s_type_key] = ctx[s_word].type


    return result


def bind_over_atom(index, first, second, ctx, unifier=None):
    result = unify_enum.NA
    f_word,  s_word  = first[index], second[index]
    f_canon, s_canon = ctx[f_word], ctx[s_word]

    match (f_word.is_var, f_canon not in ctx, ctx[f_canon] == ATOM):
        case True, True, _:
            result = unify_enum.NEXT_WORD
            ctx[f_canon] = s_word
        case True, False, True:
            result = unify_enum.NEXT_WORD
            ctx[f_canon] = s_word
        case _:
            pass

    if result is unify_enum.NEXT_WORD:
        return result

    match (s_word.is_var, s_canon not in ctx, ctx[s_canon] == ATOM):
        case True, True, _:
            result = unify_enum.NEXT_WORD
            ctx[s_canon] = f_word
        case True, False, True:
            result = unify_enum.NEXT_WORD
            ctx[s_canon] = f_word
        case _:
            pass

    return result




def unify_type_sens(index, first, second, ctx, unifier=None):
    logging.debug("Unifying Type Annotations for: {}, {}", first[index], second[index])
    result = unify_enum.NEXT_WORD
    sub_unifier = Unifier(unifier.logic.sub_logic)

    # Setup
    f_word, s_word       = first[index], second[index]
    canon_f_t, canon_s_t = f_word.type, s_word.type

    if f_word.is_var:
        canon_f_t = ctx[type_t.substitute(base=f_word.key())]

    # if s_word.is_var:
    #     canon_s_t = ctx[type_t.substitute(base=s_word.key())]

    try:
        if f_word.type != ATOM and f_word.type != canon_f_t:
            # unify f_word.type against canon
            sub_unifier(f_word.type, canon_f_t, ctx)

        if s_word.type != ATOM and s_word.type != canon_s_t:
            # unify s_word.type against canon
            sub_unifier(s_word.type, canon_s_t, ctx)

        if canon_f_t == ATOM and canon_s_t is ATOM:
            return result
        # Can the canon types unify?
        updated = sub_unifier(canon_f_t, canon_s_t, ctx)
    except TE.AcabTypingException as err:
        data = {}
        data.update(err.data)
        data.update({'types': [canon_f_t, canon_s_t]})
        raise TE.TypeConflictException(f_word, s_word, ctx=err.ctx, data=data) from err

    # Update the var in ctx, or update a tight binding of the object
    # because sentence.key() elides to a var,
    #value annotations use str()
    # TODO should this actually be f_word for the key?
    # Get the more general (ie: smaller path) type's key
    min_type= min((type_len(canon_f_t), canon_f_t),
                  (type_len(canon_s_t), canon_s_t))[1]

    if min_type == ATOM:
        return result

    if f_word.is_var:
        ctx[type_t.substitute(base=f_word.key())] = min_type
    if s_word.is_var:
        ctx[type_t.substitute(base=s_word.key())] = min_type

    if canon_f_t.is_var:
        ctx[canon_f_t] = min_type
    elif not f_word.is_var:
        # *only* the left hand / real type can be updated
        # even if it isn't a var
        f_key = str(first[:index+1])
        type_update = ValueAnnotation(DS.TYPE_INSTANCE, min_type)
        ctx[f_key] = type_update

    if canon_s_t.is_var:
        ctx[canon_s_t] = min_type

    return unify_enum.NEXT_WORD

# Fail Handlers ############################################################
def var_consistency_check(index, first, second, ctx, unifier=None):
    """
    Check variables stay consistent as you progress through the sentence
    Check words types are consistent with the context
    """
    if not (first[index].is_var or second[index].is_var):
        return unify_enum.NA
    sub_unifier = Unifier(unifier.logic.sub_logic)
    try:
        logging.debug("Checking Vars on: {}, {}", first[index], second[index])
        for side, sen in [(0, first), (1, second)]:
            word = sen[index]
            if not word.is_var:
                continue

            canon_v   = ctx[word]
            canon_w   = ctx[canon_v]
            canon_w_t = ctx[canon_w.type]
            target    = canon_w.type if canon_w.type.is_var else sen[:index+1]

            if canon_w_t == word.type:
                continue
            elif side == 0:
                sub_unifier(word.type, canon_w_t, ctx)
            else:
                sub_unifier(canon_w_t, word.type, ctx)

    except TE.AcabTypingException as err:
        raise TE.AcabUnifyVariableInconsistencyException(err.left, err.right, ctx=word) from err

    return unify_enum.NA



# Early Handlers ############################################################
def whole_type_sentence_bind(first, second, ctx, unifier=None):
    """
    Early exit if all you have is a variable or an atom
    Because this is binding a type sen,
    which is inside the sentence being unified,
    a dummy bind is used, then checked for in unify_type_sens
    """
    result = unify_enum.NA
    if first == ATOM and second == ATOM:
        return unify_enum.END

    match (first == ATOM, first.is_var, len(first), first in ctx):
        case True, _, _, False:
            # ctx['__left_bind'] = second
            result = unify_enum.END
        case False, True, 1, True if ctx[first] != second:
            raise TE.AcabUnifyVariableInconsistencyException(first, second, ctx=ctx)
        case False, True, 1, False:
            assert(first not in ctx)
            ctx[first] = second
            result = unify_enum.END
        case _:
            pass

    if result is unify_enum.END:
        return result

    match (second == ATOM, second.is_var, len(second), second[0] in ctx):
        case True, _, _, False:
            # ctx['__right_bind'] = first
            result = unify_enum.END
        case False, True, 1, True:
            canon = util.top_var(second[0], ctx)
            if ctx[canon] != first[0]:
                raise TE.AcabUnifyVariableInconsistencyException(first, second, ctx=ctx)
        case False, True, 1, False:
            assert(second[0] not in ctx)
            ctx[second[0]] = first
            result = unify_enum.END
        case _:
            pass

    return result

def whole_sentence_bind_typed(first, second, ctx, unifier=None):
    """
    Early exit if all you have is a variable
    """
    result = unify_enum.NA
    match first.is_var, first in ctx, len(first), len(second):
        case _, _, 1, 1:
           return result
        case True, True, 1, _ if ctx[first] != second:
           raise TE.AcabTypingException(first, second, ctx=ctx, msg="Left Whole Bind position already exists")
        case True, False, 1, _:
            result = unify_enum.END
            ctx[first] = second

    match second.is_var, second in ctx, len(second):
        case True, True, 1 if ctx[second] != first:
           raise TE.AcabTypingException(first, second, ctx=ctx, msg="Right Whole Bind position already exists")
        case True, False, 1:
            result = unify_enum.END
            ctx[second] = first

    return result



# Applying Substitutions#####################################################
def apply_sen_type_sub(sen, ctx) -> AT.Sentence:
    """
    Apply substitutions to a type sentence
    The important logic here is to skip any variable which reduces to ATOM
    ie: ::a.type.sen.$x, {$x: ATOM} == ::a.type.sen.$x
    """
    values = []
    for word in sen.words:
        canon_word = Bind.bind(ctx[word], ctx)
        if canon_word == ATOM:
            values.append(word)
        else:
            values.append(canon_word)

    return sen.copy(value=values)


def apply_typed_sen_sub(sen, ctx, prefix=None, top=True) -> AT.Sentence:
    """
    Apply Substitutions to sentences and each word's type sentence
    ie: a.test.sentence(::$x), {$x: ::a.type} == a.test.sentence(::a.type)
    """
    # TODO rework this
    words = []
    for index in range(len(sen)):
        word = sen[index]
        if isinstance(word, VI.Sentence_i):
            words.append(apply_typed_sen_sub(word, ctx, prefix=sen[:index], top=False))

        elif word.is_var:
            canon_var    = ctx[word] if ctx[word].is_var else word
            canon_word   = Bind.bind(canon_var, ctx)
            canon_type   = most_gen_type(Bind.bind(word.type, ctx),
                                         Bind.bind(canon_word.type, ctx))

            if len(canon_type) == 1 and isinstance(canon_type[0], VI.Sentence_i):
                canon_type = canon_type[0]

            if DS.FLATTEN in canon_var.data and canon_var.data[DS.FLATTEN] and isinstance(canon_word, VI.Sentence_i):
                words += canon_word[:]
            else:
                words.append(canon_word.copy(data={TYPE_INSTANCE : canon_type}))

        elif sen[:index+1] in ctx:
            binding = ctx[sen[:index+1]]
            assert(isinstance(binding, ValueAnnotation))
            updated      = binding(word.copy())
            canon_type   = most_gen_type(word.type, updated.type)
            if canon_type.has_var:
                canon_type = apply_sen_type_sub(canon_type, ctx)
            words.append(updated.copy(data={TYPE_INSTANCE: canon_type}))
        elif prefix is not None and prefix.key(suffix=sen[:index+1]) in ctx:
            binding = ctx[prefix.key(suffix=sen[:index+1])]
            assert(isinstance(binding, ValueAnnotation))
            updated      = binding(word.copy())
            canon_type   = most_gen_type(word.type, updated.type)
            if canon_type.has_var:
                canon_type = apply_sen_type_sub(canon_type, ctx)
            words.append(updated.copy(data={TYPE_INSTANCE: canon_type}))
        else:
            words.append(word)

    applied = sen.copy(value=words)

    return applied


def apply_types_onto_sen(sen, ctx, prefix=None, top=True) -> AT.Sentence:
    """
    Apply Substitutions to each word's type sentence
    ie: a.$y.sentence(::$x), {$x: ::a.type, $y: blah(::other)} == a.$y(::other).sentence(::a.type)
    """
    words = []
    for index in range(len(sen)):
        word = sen[index]
        if isinstance(word, VI.Sentence_i):
            words.append(apply_types_onto_sen(word, ctx, prefix=sen[:index], top=False))

        elif word.is_var:
            canon_var  = ctx[word] if ctx[word].is_var else word
            canon_word = ctx[canon_var]
            type_key   = type_t.substitute(base=canon_var.key())
            canon_type = ctx[type_key] if type_key in ctx else canon_word.type

            if len(canon_type) == 1 and isinstance(canon_type[0], VI.Sentence_i):
                canon_type = canon_type[0]

            words.append(word.copy(data={TYPE_INSTANCE: Bind.bind(canon_type, ctx)}))

        elif str(sen[:index+1]) in ctx:
            binding = ctx[str(sen[:index+1])]
            assert(isinstance(binding, ValueAnnotation))
            updated      = binding(word.copy())
            canon_type   = most_gen_type(word.type, updated.type)
            if canon_type.has_var:
                canon_type = apply_sen_type_sub(canon_type, ctx)
            words.append(updated.copy(data={TYPE_INSTANCE: canon_type}))
        elif prefix is not None and prefix.key(suffix=sen[:index+1]) in ctx:
            binding = ctx[prefix.key(suffix=sen[:index+1])]
            assert(isinstance(binding, ValueAnnotation))
            updated      = binding(word.copy())
            canon_type   = most_gen_type(word.type, updated.type)
            if canon_type.has_var:
                canon_type = apply_sen_type_sub(canon_type, ctx)
            words.append(updated.copy(data={TYPE_INSTANCE: canon_type}))
        else:
            type_sen = word.type
            applied_type_sen = Bind.bind(type_sen, ctx)

            if len(applied_type_sen) == 1 and isinstance(applied_type_sen[0], VI.Sentence_i):
                applied_type_sen = applied_type_sen[0]

            words.append(word.copy(data={TYPE_INSTANCE: applied_type_sen}))

    applied = sen.copy(value=words)

    return applied



type_as_sen_logic = UnifyLogic(
    entry_transform=None,
    early_exit=whole_type_sentence_bind,
    truncate=util.sen_extend,
    sieve=[
        # Basic Type Handling
        bind_over_atom,
        # Variables
        suf.var_handler,
        # Values
        suf.equality_check,
        suf.check_modality,
        suf.sentence_recursion,
        # Fail
        suf.next_word
        ],
    apply=None)

type_sen_unify = Unifier(type_as_sen_logic)

# a.b.c(::d.e.f)
typed_sen_logic = UnifyLogic(
    entry_transform=None, #lambda x,y,c: (x, y, gen_type_vars(x, y, c)),
    early_exit=whole_sentence_bind_typed,
    truncate=util.sen_extend,
    sieve=[
        # Handle Variables
        var_handler,
        # Handle Values
        suf.equality_check,
        suf.check_modality,
        typed_sentence_recursion,
        # var_consistency_check,
        # Handle types:
        atom_types_satisifed,
        fix_types,
        unify_type_sens,
        # Fail
        suf.fail_handler],
    apply=apply_typed_sen_sub,
    sub_logic=type_as_sen_logic
    )

type_unify = Unifier(typed_sen_logic)
