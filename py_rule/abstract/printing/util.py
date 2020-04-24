"""
Utilities for printing out information
"""
from py_rule import util

#Setup
def setup_modal_lookups(a_dict, reset=False):
    if reset:
        MODAL_LOOKUPS.clear()
    assert(not any([k in MODAL_LOOKUPS for k in a_dict.keys()]))
    MODAL_LOOKUPS.update(a_dict)

def setup_statement_lookups(a_dict, reset=False):
    if reset:
        STATEMENT_LOOKUPS.clear()
    assert(not any([k in STATEMENT_LOOKUPS for k in a_dict.keys()])), a_dict
    STATEMENT_LOOKUPS.update(a_dict)

def setup_primitive_lookups(a_dict, reset=False):
    if reset:
        TYPE_WRAPS.clear()
    assert(not any([k in TYPE_WRAPS for k in a_dict.keys()])), a_dict
    assert(all([callable(x) for x in a_dict.values()]))
    TYPE_WRAPS.update(a_dict)

# TOP LEVEL UTILITIES
def print_value(value, leaf=False, **kwargs):
    # TODO: handle printing a primitive value

    val = value.name

    # setup the value type
    type_str = None
    if util.VALUE_TYPE_S in value._data:
        type_str = value._data[util.VALUE_TYPE_S]

    # Format core value if it is a primitive
    if type_str in TYPE_WRAPS:
        val = TYPE_WRAPS[type_str](val)

    # Wrap binding
    if value.is_at_var:
        val = _wrap_at_var(val)
    elif value.is_var:
        val = _wrap_var(val)

    # Wrap constraints
    if util.CONSTRAINT_S in value._data:
        val = _wrap_constraints(val, value._data[util.CONSTRAINT_S])

    # Wrap modal Operator
    if not leaf and util.OPERATOR_S in value._data:
        val = _wrap_modal_operator(val, value._data[util.OPERATOR_S])

    return val

def print_operator(operator, op_fix=0, wrap_vars=False):
    """ Op Fix is the count of vars to print before printing the op.
    eg: op_fix=0 : + 1 2 3 ...
        op_fix=2 : 1 2 + 3 ...
    """
    # Format the vars of the operator
    the_vars = [x.pprint(leaf=True) for x in operator._vars]

    if wrap_vars:
        # Wrap variables in parens if an action
        assert(op_fix == 0)
        val = "{}({})".format(operator.op, ",".join(the_vars))
    else:
        # Don't wrap comps or transforms
        head = " ".join([str(x) for x in the_vars[:op_fix]] + [operator.op])
        tail = "".join([str(x) for x in the_vars[op_fix:]])
        val = "{} {}".format(head, tail)

    # Wrap a rebind if there is one
    if hasattr(operator, "_rebind"):
        val = _wrap_rebind(val, operator._rebind)

    return val

def print_sequence(seq, join_str="", leaf=True, **kwargs):
    if not bool(seq):
        return ""

    words = []
    if len(seq) > 1:
        words = [x.pprint(leaf=False) for x in seq.words[:-1]]

    last_word = [seq.words[-1].pprint(leaf=leaf)]

    val = join_str.join(words + last_word)

    # Wrap as Query
    if util.QUERY_S in seq._data:
        val = _wrap_question(val)

    # Wrap as negated
    if util.NEGATION_S in seq._data and seq._data[util.NEGATION_S]:
        val = _wrap_negation(val)

    # Wrap fallback vars
    if util.FALLBACK_S in seq._data and bool(seq._data[util.FALLBACK_S]):
        val = _wrap_fallback(val, seq._data[util.FALLBACK_S])

    return val

def print_container(container, join_str="\n\t", **kwargs):
    # TODO is this necessary now containers are statements?
    the_clauses = [x.pprint(**kwargs) for x in container.clauses]
    return join_str.join(the_clauses)

def print_statement(statement, is_structured=False, has_end=True, **kwargs):

    head, body = statement.pprint_has_content

    val = statement._name
    #TODO handle operator definition by controlling head printing
    # ie: λ:: name(vars): x.y.z => blah
    val = _wrap_colon(val)

    if statement.type in STATEMENT_LOOKUPS:
        val = _wrap_statement_type(val, statement.type)

    if bool(statement._vars):
        val = _wrap_var_list(val, statement._vars)

    if bool(statement._tags):
        val = _wrap_tags(val, statement._tags)

    # Add the statement body, which is specific to each statement type
    val = statement.pprint_body(val)

    if has_end:
        val = _wrap_end(val, newline=head or body)

    return val

def print_fallback(fallback_list):
    return ", ".join(["{}:{}".format(_wrap_var(x[0]), x[1].pprint())
                      for x in fallback_list])


# PRINTING COMPONENTS
def _wrap_str(value):
    assert(isinstance(value, str))
    return '"{}"'.format(value)

def _wrap_float(value):
    return str(value).replace('.', util.DECIMAL_S)

def _wrap_int(value):
    return str(value)
def _wrap_regex(value):
    assert(isinstance(value, str))
    val = "/{}/".format(value)
    return val

def _wrap_var(value):
    assert(isinstance(value, str))
    return util.VAR_SYMBOL_S + value

def _wrap_at_var(value):
    assert(isinstance(value, str))
    return util.AT_VAR_SYMBOL_S + value

def _wrap_constraints(value, constraints):
    assert(isinstance(value, str))
    assert(isinstance(constraints, list))
    cons_strs = ", ".join([x.pprint() for x in constraints])
    return value + "({})".format(cons_strs)

def _wrap_modal_operator(value, op):
    assert(isinstance(value, str))
    assert(op in MODAL_LOOKUPS)
    return value + MODAL_LOOKUPS[op]

def _wrap_rebind(value, rebind, is_sugar=False):
    arrow = "->"
    if rebind is None:
        return value
    if is_sugar:
        arrow = "=>"

    return "{} {} {}".format(value,
                             arrow,
                             print_value(rebind, leaf=True))

def _wrap_question(value):
    return "{}{}".format(value, util.QUERY_SYMBOL_S)

def _wrap_negation(value):
    return "{}{}".format(util.NEGATION_SYMBOL_S, value)

def _wrap_fallback(value, fallback_list):
    return "{} || {}".format(value, print_fallback(fallback_list))

def _wrap_tags(value, tags, sep="\n\t"):
    return "{}{}{}".format(value, sep, ", ".join(sorted([util.TAG_SYMBOL_S + x for x in tags])))

def _maybe_wrap(value, maybeNone, sep=None):
    if maybeNone is None:
        return (value, False)

    return (value + sep + maybeNone.pprint(as_container=True), True)

def _wrap_colon(value, newline=False):
    tail = ""
    if newline:
        tail = "\n"

    return "{}:{}".format(value, tail)

def _wrap_end(value, newline=True):
    if newline:
        return "{}\n{}".format(value, util.END_S)
    else:
        return "{} {}".format(value, util.END_S)

def _wrap_statement_type(val, type_str):
    return "{} (::{})".format(val,
                              STATEMENT_LOOKUPS[type_str])

def _wrap_var_list(val, the_vars, newline=False):
    head = ""
    if newline:
        head = "\n"
    return "{}{}\t | {} |\n".format(val, head, ", ".join([_wrap_var(x) for x in the_vars]))



# TODO: register / add to these
MODAL_LOOKUPS = {}
STATEMENT_LOOKUPS = {}
TYPE_WRAPS = {util.STRING_S : _wrap_str,
              util.FLOAT_S  : _wrap_float,
              util.INT_S    : _wrap_int,
              util.REGEX_S  : _wrap_regex}
