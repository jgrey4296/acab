import logging as root_logger

import acab.util as util

from acab.abstract.type_base import TypeInstance

logging = root_logger.getLogger(__name__)

ROOT_S        = util.ROOT_S
BIND_S        = util.BIND_S
SEN_S         = util.SEN_S

VALUE_TYPE_S  = util.VALUE_TYPE_S
TYPE_DEF_S    = "type_definition"
OP_DEF_S      = "operator_definition"
SUM_DEF_S     = "sum_definition"
STRUCT_S      = "structure"
TVAR_S        = "type_vars"
DELIM_S       = ", "
ARG_S         = "arguments"
SYNTAX_BIND_S = "syntax_bind"

TYPE_DEFINITION = TypeInstance(primitive=[TYPE_DEF_S])
OPERATOR_DEFINITION = TypeInstance(primitive=[OP_DEF_S])
SUM_DEFINITION = TypeInstance(primitive=[SUM_DEF_S])

def has_equivalent_vars_pred(node):
    """ A Predicate to use with Trie.get_nodes
    Finds nodes with multiple vars as children that can be merged """
    if node.name == util.ROOT_S:
        return False
    var_children = [x for x in node._children.values() if x.is_var]
    return len(var_children) > 1
