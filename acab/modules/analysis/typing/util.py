import logging as root_logger
import pyparsing as pp

from acab.config import AcabConfig

from acab.abstract.core.type_system import AcabTypeSystem
from acab.abstract.core.type_base import TypeInstance

from acab.abstract.parsing.util import s

logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()

ROOT_S        = util("Data.Struct", "ROOT_S")
BIND_S        = util("Parsing.Structure", "BIND_S")
SEN_S         = util("Parsing.Structure", "SEN_S")
VALUE_TYPE_S  = util("Parsing.Structure", "VALUE_TYPE_S")
ARG_S         = util("Parsing.Structure", "ARG_S")
OPERATOR_S    = util("Parsing.Structure", "OPERATOR_S")
TYPE_DEF_S    = util("Module.Typing", "TYPE_DEF_S")
OP_DEF_S      = util("Module.Typing", "OP_DEF_S")
SUM_DEF_S     = util("Module.Typing", "SUM_DEF_S")
STRUCT_S      = util("Module.Typing", "STRUCT_S")
TVAR_S        = util("Module.Typing", "TVAR_S")
SYNTAX_BIND_S = util("Module.Typing", "SYNTAX_BIND_S")

SUM_HEAD       = s(util("Module.Typing.Symbols", "SUM_HEAD_S", action=AcabConfig.actions_e.KEYWORD))
STRUCT_HEAD    = s(util("Module.Typing.Symbols", "STRUCTURE_S", action=AcabConfig.actions_e.KEYWORD))
TYPE_CLASS_HEAD = s(util("Module.Typing.Symbols", "TYPE_CLASS_S", action=AcabConfig.actions_e.KEYWORD))
FUNC_HEAD      = s(pp.Word(util("Visual.Symbols", "FUNC_SYMBOL_S")))

DELIM_S       = util("Module.Typing", "DELIM_S", action=AcabConfig.actions_e.STRIPQUOTE)

# TODO make these registrations
TYPE_DEFINITION = TypeInstance(path=[TYPE_DEF_S])
SUM_DEFINITION = TypeInstance(path=[SUM_DEF_S])
OPERATOR_DEFINITION = TypeInstance(path=[OP_DEF_S])
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
