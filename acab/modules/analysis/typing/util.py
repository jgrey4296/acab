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
