import py_rule.util as util
import logging as root_logger
logging = root_logger.getLogger(__name__)

ROOT_S = util.ROOT_S
BIND_S = util.BIND_S
SEN_S = util.SEN_S

TYPE_DEC_S = "type_dec"
TYPE_DEF_S = "type_def"
OP_DEF_S = "op_def"
STRUCT_S    = "structure"
TVAR_S     = "type_vars"
DELIM_S    = ", "
ARG_S      = "arguments"

def has_equivalent_vars_pred(node):
    """ A Predicate to use with Trie.get_nodes
    Finds nodes with multiple vars as children that can be merged """
    if node._value == util.ROOT_S:
        return False
    var_children = [x for x in node._children.values() if x._is_var]
    return len(var_children) > 1


def is_var(node):
    return util.BIND_S in node._data and node._data[util.BIND_S]
