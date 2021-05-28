import logging as root_logger
import pyparsing as pp

from acab.abstract.config.config import AcabConfig

from acab.abstract.core.values import Sentence

from acab.abstract.parsing.consts import s

logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

ROOT_S          = config.value("Data", "ROOT")

BIND_S          = config.value("Value.Structure", "BIND")
TYPE_INSTANCE_S = config.value("Value.Structure", "TYPE_INSTANCE")
ARG_S           = config.value("Value.Structure", "PARAMS")
OPERATOR_S      = config.value("Value.Structure", "OPERATOR")
SEN_S           = config.value("Value.Structure", "SEN")

TYPE_DEF_S      = config.value("Typing.Primitives", "TYPE_DEF")
OP_DEF_S        = config.value("Typing.Primitives", "OP_DEF")
SUM_DEF_S       = config.value("Typing.Primitives", "SUM_DEF")
STRUCT_S        = config.value("Typing.Primitives", "STRUCT")
TVAR_S          = config.value("Typing.Primitives", "TVAR")
SYNTAX_BIND_S   = config.value("Typing.Primitives", "SYNTAX_BIND")

PARAM_JOIN_S    = config.value("Print.Patterns", "PARAM_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])

SUM_HEAD        = s(config.value("Symbols", "SUM", actions=[AcabConfig.actions_e.KEYWORD]))
STRUCT_HEAD     = s(config.value("Symbols", "STRUCTURE", actions=[AcabConfig.actions_e.KEYWORD]))
TYPE_CLASS_HEAD = s(config.value("Symbols", "TYPE_CLASS", actions=[AcabConfig.actions_e.KEYWORD]))
FUNC_HEAD       = s(pp.Word(config.value("Symbols", "FUNC")))

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
