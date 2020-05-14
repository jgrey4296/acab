"""
Utilities for printing out information
"""
from py_rule import util
from collections import defaultdict

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


def default_opts(*args, join="", **kwargs):
    opts = defaultdict(lambda: False)
    for x in args:
        opts[x] = True
    for x,y in kwargs.items():
        opts[x] = y
    opts['join'] = join
    return opts

# TOP LEVEL UTILITIES
def print_value(value, opts):

    val = value.name
    if 'internal_value' in opts:
        val = opts['internal_value']

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
    if not opts['leaf'] and util.OPERATOR_S in value._data:
        val = _wrap_modal_operator(val, value._data[util.OPERATOR_S])

    return val

def print_operator(operator, opts):
    """ Op Fix is the count of vars to print before printing the op.
    eg: op_fix=0 : + 1 2 3 ...
        op_fix=2 : 1 2 + 3 ...
    """
    # Format the vars of the operator
    op_fix = opts['op_fix']
    wrap_params = opts['wrap']
    if not op_fix:
        op_fix = 0

    def_op = default_opts()
    def_op['leaf'] = True
    the_params = [x.pprint(def_op) for x in operator._params]

    if wrap_params:
        # Wrap params in parens if an action
        assert(op_fix == 0)
        val = "{}({})".format(operator.op, ",".join(the_params))
    else:
        # Don't wrap comps or transforms
        head = " ".join([str(x) for x in the_params[:op_fix]] + [operator.op])
        tail = "".join([str(x) for x in the_params[op_fix:]])
        val = "{} {}".format(head, tail)

    # Wrap a rebind if there is one
    if hasattr(operator, "_rebind"):
        val = _wrap_rebind(val, operator._rebind)

    return val

def print_sequence(seq, opts):
    join_str = opts['join']

    if not bool(seq):
        return ""

    words = []
    if len(seq) > 1:
        def_op = default_opts()
        words = [x.pprint(def_op) for x in seq.words[:-1]]

    last_word = [seq.words[-1].pprint(opts)]

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

def print_container(container, opts):
    opts_copy = opts.copy()
    the_clauses = [x.pprint(opts_copy) for x in container.clauses]
    if 'join' not in opts:
        opts['join'] = "\n\t"

    return opts['join'].join(the_clauses)

def print_statement(statement, opts):

    head, body = statement.pprint_has_content

    val = statement._name
    val = _wrap_colon(val)

    if statement.type in STATEMENT_LOOKUPS:
        val = _wrap_statement_type(val, statement.type)

    if bool(statement._vars):
        val = _wrap_var_list(val, statement._vars)

    if bool(statement._tags):
        val = _wrap_tags(val, statement._tags)

    # Add the statement body, which is specific to each statement type
    val = statement.pprint_body(val)

    if opts['leaf'] or opts['end']:
        val = _wrap_end(val, newline=head or body)

    return val

def print_fallback(fallback_list):
    def_op = default_opts()
    return ", ".join(["{}:{}".format(_wrap_var(x[0]), x[1].pprint(def_op))
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
    cons_strs = ", ".join([x.pprint(default_opts()) for x in constraints])
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

    def_op = default_opts('leaf')
    return "{} {} {}".format(value,
                             arrow,
                             print_value(rebind, def_op))

def _wrap_question(value):
    return "{}{}".format(value, util.QUERY_SYMBOL_S)

def _wrap_negation(value):
    return "{}{}".format(util.NEGATION_SYMBOL_S, value)

def _wrap_fallback(value, fallback_list):
    return "{} || {}".format(value, print_fallback(fallback_list))

def _wrap_tags(value, tags, sep="\n\t"):
    tags_s = [x.name for x in tags]
    return "{}{}{}".format(value, sep, ", ".join(sorted([util.TAG_SYMBOL_S + x for x in tags_s])))

def _maybe_wrap(value, maybeNone, sep=None):
    if maybeNone is None:
        return (value, False)
    return (value + sep + maybeNone.pprint(default_opts('container')), True)

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
