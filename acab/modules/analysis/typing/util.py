from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import logging as root_logger
import pyparsing as pp
from uuid import uuid1

from acab.core.config.config import AcabConfig
from acab import types as AT

from acab.core.data.values import Sentence

from acab.core.parsing.consts import s, s_key

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

UnifyResult = Dict[str, AT.Value]
def unify_sentences(first: AT.Sentence, second: AT.Sentence) -> UnifyResult:
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
    result : Dict[Any, Any] = {}
    if len(first) != len(second):
        raise AcabSemanticException("Unify Failure: Length Mismatch")

    for a,b in zip(first.words, second.words):
        if a.is_var and a.key() in result and result[a.key()] != b:
            raise AcabSemanticException("Unify Failure: Key Mismatch")
        elif b.is_var and b.key() in result and result[b.key()] != a:
            raise AcabSemanticException("Unify Failure: Key Mismatch")
        elif a.is_var:
            result[a.key()] = b
        elif b.is_var:
            result[b.key()] = a
        elif a == b:
            continue
        else:
            raise AcabSemanticException("Unify Failure: No Applicable Match")

    return result


LaxUnifyResult = Tuple[UnifyResult, AT.Sentence, AT.Sentence]
def lax_unify_sentences(first: AT.Sentence, second: AT.Sentence) -> LaxUnifyResult:
    """

    """
    min_len = min(len(first), len(second))
    subs = unify_sentences(first[:min_len], second[:min_len])
    remL = first[min_len:]
    remR = second[min_len:]

    assert(len(remL) == 0 or len(remR) == 0)
    return (subs, remL, remR)

def subtype_relation(sen1: AT.Sentence, sen2: AT.Sentence) -> bool:
    """
    sub( a.b.c, ATOM)   -> False
    sub( a.b.c, a.d)    -> False
    sub( a.b.c, a.b)    -> True
    sub( a.b.c, a.b.$x) -> True
    sub( a.b.$x, a.b)   -> True
    sub( a.$x.c, a.b)   -> True?

    """
    if sen2 == "_:ATOM":
        return False
    if sen1 == "_:ATOM":
        return True

    try:
        lax_unify_sentences(sen1, sen2)
    except AcabSemanticException:
        return False

    return True


def apply_sentence_types(first: AT.Sentence, second: AT.Sentence) -> AT.Sentence:
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
    result = []

    for a,b in zip(first.words, second.words):
        is_subtype = subtype_relation(a.type, b.type)
        if (a.is_var or b.is_var or a.name == b.name) and is_subtype:
            result.append(a.copy(data={'TYPE_INSTANCE': b.type.copy()}))
        else:
            result.append(a)

    return first.copy(value=result)
