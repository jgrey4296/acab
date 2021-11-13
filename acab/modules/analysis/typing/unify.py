#!/usr/bin/env python3
import logging as root_logger
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from uuid import uuid1

import pyparsing as pp
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.data.values import Sentence, AcabValue
from acab.core.parsing.consts import s, s_key
from acab.modules.context.context_set import ContextInstance, MutableContextInstance
from . import type_exceptions as TE

logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

ROOT_S          = config.prepare("Data", "ROOT")()

BIND_S          = config.prepare("Value.Structure", "BIND")()
TYPE_INSTANCE_S = config.prepare("Value.Structure", "TYPE_INSTANCE")()
ARG_S           = config.prepare("Value.Structure", "PARAMS")()
OPERATOR_S      = config.prepare("Value.Structure", "OPERATOR")()
SEN_S           = config.prepare("Value.Structure", "SEN")()

TYPE_DEF_S      = config.prepare("Typing.Primitives", "TYPE_DEF")()
OP_DEF_S        = config.prepare("Typing.Primitives", "OP_DEF")()
SUM_DEF_S       = config.prepare("Typing.Primitives", "SUM_DEF")()
STRUCT_S        = config.prepare("Typing.Primitives", "STRUCT")()
TVAR_S          = config.prepare("Typing.Primitives", "TVAR")()
SYNTAX_BIND_S   = config.prepare("Typing.Primitives", "SYNTAX_BIND")()

# TODO make these registrations
TYPE_DEFINITION     = Sentence.build([TYPE_DEF_S])
SUM_DEFINITION      = Sentence.build([SUM_DEF_S])
OPERATOR_DEFINITION = Sentence.build([OP_DEF_S])
# TODO TYPE CLASS

from acab.error.semantic_exception import AcabSemanticException
unify_transform = Callable[[Any, Any], Any]

def gen_var() -> Callable[[], AT.Value]:
    """ A Simple Generator of guaranteed new Variables """
    counter = 0
    def wrapped() -> AT.Value:
        nonlocal counter
        new_name = "GenVar_{}".format(counter)
        return Sentence.build([AcabValue.safe_make(new_name, data={"BIND":True})])

    return wrapped



def basic_unify_transform(a, b, gamma_p):
    # Expand Vars
    a_p = gamma_p[a]
    b_p = gamma_p[b]

    ## --  Unify decision:
    # Unifiable
    if a_p.is_var:
        gamma_p[a_p.key()] = b_p
    elif b_p.is_var:
        gamma_p[b_p.key()] = a_p
    # Equal
    elif a_p == b_p:
        # TODO handle var args in the type constructors,
        # so recursively unify
        pass
    # Not Equal
    elif a == a_p:
        raise TE.TypeUnifyException(a, b, (a, (b, b_p)), gamma_p)
    else:
        raise TE.TypeUnifyException(a, b, (b, (a, a_p)), gamma_p)

    return None

gen_f  = gen_var()
def type_unify_transform(a, b, gamma_p):
    if b.type == "_:ATOM" and not b.is_var:
        return a

    l_type = a.type
    if not l_type.is_var and l_type == "_:ATOM":
        l_type = gen_f()
        a = a.copy(data={'TYPE_INSTANCE': l_type})
        gamma_p[a] = a

    # Unify types / check l < r
    #
    sen_r, ctx_r = unify_sentence_pair(l_type, b.type,
                                 transform=basic_unify_transform,
                                 gamma=gamma_p)

    # TODO: should mod_a be inserted into a somewhere

    return a

def type_remainder_transform(a, gp_f):
    reified = gp_f[gp_f[a].type]
    current = gp_f[a.type]
    if reified != current and a.type != "_:ATOM":
        raise TE.TypeConflictException(a, a, (reified, a.type), gp_f)
    return a


def unify_sentence_pair(first: AT.Sentence,
                        second: AT.Sentence,
                        transform:Callable[[Any], Any],
                        gamma:AT.CtxIns=None,
                        strict=False,
                        remainder_op=None) -> (AT.Sentence, AT.CtxIns):
    """
    Unify a pair of sentences, word by word.

    Can be `strict`, which enforces length equality.
    If Not `strict`, can unify an entire sentence to a lone variable

    takes a `gamma` or generates one.

    can `remainder_op` the tail of unequal sentences against gamma

    """

    if not strict and first.is_var and gamma is None:
        return first, ContextInstance({first[0].key(): second})
    elif not strict and first.is_var and gamma is not None:
        return first, gamma.bind_dict({first[0].key(): second})
    elif strict and not (len(first) == len(second)):
        raise TE.AcabTypingException()

    if gamma is None:
        gamma = ContextInstance()

    # Gamma'
    gamma_p = MutableContextInstance(None, gamma)
    collect = []
    with gamma_p:
        for a,b in zip(first.words, second.words):
            # Raises unify exception or add's to gamma_p
            collect.append(transform(a,b, gamma_p))

    # Add anything not touched
    gp_f = gamma_p.finish()

    if len(second) < len(first) and remainder_op:
        # check remainder against gamma
        for word in first[len(second):]:
            collect.append(remainder_op(word, gp_f))

    if bool(collect):
        return first.copy(value=[x for x in collect if bool(x)]), gp_f
    else:
        return first, gp_f
