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

PARAM_JOIN_S    = config.prepare("Print.Patterns", "PARAM_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])()

SUM_HEAD        = s_key(config.prepare("Symbols", "SUM")())
STRUCT_HEAD     = s_key(config.prepare("Symbols", "STRUCTURE")())
TYPE_CLASS_HEAD = s_key(config.prepare("Symbols", "TYPE_CLASS")())
FUNC_HEAD       = s(pp.Word(config.prepare("Symbols", "FUNC")()))

# TODO make these registrations
TYPE_DEFINITION     = Sentence.build([TYPE_DEF_S])
SUM_DEFINITION      = Sentence.build([SUM_DEF_S])
OPERATOR_DEFINITION = Sentence.build([OP_DEF_S])
# TODO TYPE CLASS

STRUCT_HEAD.setName("StructHead")
FUNC_HEAD.setName("FuncHead")

def has_equivalent_vars_pred(node):
    """ A Predicate to use with Trie.get_nodes
    Finds nodes with multiple vars as children that can be merged """
    if node.name is ROOT_S:
        return False
    var_children = [x for x in node._children.values() if x.is_var]

    return len(var_children) > 1

def create_type_var(tc, base_name):
    # Create a new var name
    assert(isinstance(base_name, str))
    var_name = str(uuid1())
    return tc._variables.add([base_name, var_name], [])

def pattern_match_type_signature(head, available):
    if head.type is None:
        return available

    return [x for x in available if x.type_instance is None
            or head.type_instance == x.type_instance]



from acab.error.semantic_exception import AcabSemanticException

def gen_var() -> Callable[[], AT.Value]:
    """ A Simple Generator of guaranteed new Variables """
    counter = 0
    def wrapped() -> AT.Value:
        nonlocal counter
        new_name = "GenVar_{}".format(counter)
        return Sentence.build([AcabValue.safe_make(new_name, data={"BIND":True})])

    return wrapped

LaxUnifyResult = Tuple[AT.CtxIns, AT.Sentence, AT.Sentence]
def unify_sentences(first: AT.Sentence, second: AT.Sentence, gamma:AT.CtxIns=None) -> AT.CtxIns:
    """
    Strict Unify two sentences, returning a dictionary of substitutions

    a.b.c   ∪ a.b.$x -> {x : c}
    a.b.$x  ∪ a.b.$y -> {x : $y}
    a.$x.$y ∪ a.b.c  -> {x : b, y : c}
    a.b.c   ∪ a.b.d  -> error
    a.b.c   ∪ q      -> error

    Maybe:
    a.b.c   ∪ a.b.c.d.e -> {tail: d.e}

    """
    if len(first) != len(second):
        raise TE.AcabTypingException()

    if gamma is None:
        gamma = ContextInstance()

    # Gamma'
    gp = MutableContextInstance(None, gamma)
    with gp:
        for a,b in zip(first.words, second.words):

            a_p = gp[a]
            b_p = gp[b]

            if a_p.is_var:
                gp[a_p.key()] = b_p
            elif b_p.is_var:
                gp[b_p.key()] = a_p
            elif a_p == b_p:
                # TODO handle var args in the type constructors,
                # so recursively unify
                continue
            elif a == a_p:
                raise TE.TypeUnifyException(first, second, (a, (b, b_p)), gp)
            else:
                raise TE.TypeUnifyException(first, second, (b, (a, a_p)), gp)

    return gp.finish()


def lax_unify(first: AT.Sentence, second: AT.Sentence, gamma:AT.CtxIns=None) -> AT.CtxIns:
    """
    unify, but also return how much of the sentences remain
    """
    # handle unify([$x], [a.b.c])
    if first.is_var and gamma is None:
        return ContextInstance({first[0].key(): second})
    elif first.is_var and gamma is not None:
        return gamma.bind_dict({first[0].key(): second})


    min_len = min(len(first), len(second))
    gammap = unify_sentences(first[:min_len], second[:min_len], gamma=gamma)

    return gammap


def unify_types(first: AT.Sentence, second: AT.Sentence, gamma:AT.CtxIns=None) -> Tuple[AT.Sentence, AT.CtxIns]:
    """
    given two sentences, apply latter types to former
    a.b.d
    a.b(::blah).c(::bloo)
    ->
    a.b(::blah).d

    a.b.d
    a.b(::blah).$x(::bloo)
    ->
    a.b(::blah).d(::bloo)

    a.b.d(::awf)
    a.b(::blah).$x(::bloo)
    ->
    awf < bloo: a.b(::blah).d(::bloo)
    else error

    """

    if gamma is None:
        gamma = ContextInstance()

    gp     = MutableContextInstance(None, gamma)
    gen_f  = gen_var()
    result = []
    with gp:
        for a,b in zip(first.words, second.words):

            if b.type == "_:ATOM" and not b.is_var:
                result.append(a)
                continue

            l_type = a.type
            if not l_type.is_var and l_type == "_:ATOM":
                l_type = gen_f()
                a = a.copy(data={'TYPE_INSTANCE': l_type})

            # Unify types / check l < r
            lax_unify(l_type, b.type, gamma=gp)

            if a.is_var:
                gp[a] = a

            result.append(a)


    # Add anything not touched
    gp_f = gp.finish()

    if len(second) < len(first):
        # check remainder against gamma
        for word in first[len(second):]:
            reified = gp_f[gp_f[word].type]
            current = gp_f[word.type]
            if reified != current and word.type != "_:ATOM":
                raise TE.TypeConflictException(first, second, (reified, word.type), gp_f)
            result.append(word)

    return first.copy(value=result), gp.finish()
