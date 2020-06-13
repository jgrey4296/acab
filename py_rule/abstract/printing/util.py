"""
Utilities for printing out information
"""
from py_rule import util
from collections import defaultdict

#Setup
# TODO register additional constraints
def register_modal(a_dict, reset=False):
    global MODAL_LOOKUPS
    if reset:
        MODAL_LOOKUPS.clear()
    assert(not any([k in MODAL_LOOKUPS for k in a_dict.keys()]))
    MODAL_LOOKUPS.update(a_dict)

def register_statement(a_dict, reset=False):
    global STATEMENT_LOOKUPS
    if reset:
        STATEMENT_LOOKUPS.clear()
    assert(not any([k in STATEMENT_LOOKUPS for k in a_dict.keys()])), a_dict
    STATEMENT_LOOKUPS.update(a_dict)

def register_primitive(a_dict, reset=False):
    global TYPE_WRAPS
    if reset:
        TYPE_WRAPS.clear()
    assert(not any([k in TYPE_WRAPS for k in a_dict.keys()])), a_dict
    assert(all([callable(x) for x in a_dict.values()]))
    TYPE_WRAPS.update(a_dict)

def register_constraint(*constraints, reset=False):
    global REGISTERED_CONSTRAINTS
    if reset:
        REGISTERED_CONSTRAINTS = set([util.CONSTRAINTS_S])

    REGISTERED_CONSTRAINTS.update(constraints)

def register_class(cls, func):
    global REGISTERED_PPRINTS
    assert(isinstance(cls, type))
    assert(callable(func))
    assert(cls not in REGISTERED_PPRINTS)
    REGISTERED_PPRINTS[cls] = func

def default_opts(*args, **kwargs):
    opts = defaultdict(lambda: False)
    for x in args:
        opts[x] = True
    for x,y in kwargs.items():
        opts[x] = y
    return opts


# TOP LEVEL UTILITIES
def pprint(obj, opts=None, **kwargs):
    """ Top Level PPrint Routine, looks up class type
    in registered pprints and calls that, with default opts if necessary """
    cls = obj.__class__

    if opts is None:
        opts = default_opts()

    opts.update(kwargs)

    # Best
    if cls in REGISTERED_PPRINTS:
        return REGISTERED_PPRINTS[cls](obj, opts)

    # Closest
    parent = cls.__base__
    while parent is not None:
        if parent in REGISTERED_PPRINTS:
            return REGISTERED_PPRINTS[parent](obj, opts)
        parent = parent.__base__

    # Default
    return str(obj)



def print_value(value, opts):

    val = pprint(value.value, opts)

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

    # Wrap constraints (TODO *and* type assigments)
    val = _wrap_constraints(val, value._data)

    # Wrap modal Operator
    print_modal = opts['modal']
    if print_modal and util.OPERATOR_S in value._data:
        val = _wrap_modal_operator(val, value._data[util.OPERATOR_S])

    return val

def print_sequence(seq, opts):
    join_str = opts['seq_join']
    if not join_str:
        join_str = ""

    if not bool(seq):
        return ""

    modal_backup = opts['modal']
    opts['modal'] = True
    words = []
    if len(seq) > 1:
        words = [print_value(x, opts) for x in seq.words[:-1]]

    # TODO: convert opts to a class, make it use context entry and exit
    opts['modal'] = False
    last_word = [seq.words[-1].pprint(opts)]
    opts['modal'] = modal_backup

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
    the_clauses = [x.pprint(opts) for x in container.clauses]

    join_str = opts['container_join']
    if not join_str:
        join_str = "\n\t"

    return "{}\n".format(join_str.join(the_clauses))

def print_statement(statement, opts):

    head, body = statement.pprint_has_content

    val = statement._name

    val = _wrap_colon(val)

    if statement.type in STATEMENT_LOOKUPS:
        val = _wrap_statement_type(val, statement.type)
    else:
        val += "\n"

    if bool(statement._vars):
        val = _wrap_var_list(val, statement._vars)

    if bool(statement._tags):
        val = _wrap_tags(val, statement._tags)

    # Add the statement body, which is specific to each statement type
    if any(statement.pprint_has_content):
        val = statement.pprint_body(val)
        val = _wrap_end(val)
    else:
        val = _wrap_end(val.strip() + " ")

    return val

def print_fallback(fallback_list):
    return ", ".join(["{}:{}".format(_wrap_var(x[0]), x[1].pprint())
                      for x in fallback_list])


def print_operator(operator, opts):
    """ Op Fix is the count of vars to print before printing the op.
    eg: op_fix=0 : + 1 2 3 ...
        op_fix=2 : 1 2 + 3 ...
    """
    # TODO fix this
    op_fix = operator._op_position
    op_str = "\\{}".format(operator.op)

    join_str = opts['join']
    if not join_str:
        join_str = " "

    the_params = [x.pprint() for x in operator._params]

    # Don't wrap comps or transforms
    head = [str(x) for x in the_params[:op_fix]]
    tail = [str(x) for x in the_params[op_fix:]]
    val = join_str.join(head + [op_str] + tail)

    return val

def print_operator_rebind(operator, opts):
    val = print_operator(operator, opts)
    val = _wrap_rebind(val, operator._rebind)
    return val

def print_operator_wrap(operator, opts):
    # Format the vars of the operator
    op_fix = operator._op_position
    join_str = opts['join']
    if not join_str:
        join_str = ", "

    the_params = [x.pprint() for x in operator._params]
    val = "{}({})".format(operator.op, join_str.join(the_params))

    return val


# PRINTING COMPONENTS
def _wrap_str(value, opts=None):
    assert(isinstance(value, str))
    return '"{}"'.format(value)

def _wrap_float(value, opts=None):
    return str(value).replace('.', util.DECIMAL_S)

def _wrap_int(value, opts=None):
    return str(value)
def _wrap_regex(value, opts=None):
    assert(isinstance(value, str))
    val = "/{}/".format(value)
    return val


def _wrap_var(value):
    assert(isinstance(value, str))
    return util.VAR_SYMBOL_S + value

def _wrap_at_var(value):
    assert(isinstance(value, str))
    return util.AT_VAR_SYMBOL_S + value

def _wrap_constraints(value, data):
    assert(isinstance(value, str))
    assert(isinstance(data, dict))

    constraints = []
    for x in REGISTERED_CONSTRAINTS:
        if x in data:
            if isinstance(data[x], list):
                constraints += data[x]
            else:
                constraints.append(data[x])

    result = value
    if bool(constraints):
        cons_strs = ", ".join([pprint(x) for x in constraints])
        result += "({})".format(cons_strs)
    return result

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
                             rebind.pprint())

def _wrap_question(value):
    return "{}{}".format(value, util.QUERY_SYMBOL_S)

def _wrap_negation(value):
    return "{}{}".format(util.NEGATION_SYMBOL_S, value)

def _wrap_fallback(value, fallback_list):
    return "{} || {}".format(value, print_fallback(fallback_list))

def _wrap_tags(value, tags, sep="\t"):
    tags_s = [str(x) for x in tags]
    return "{}{}{}\n\n".format(value, sep, ", ".join(sorted([util.TAG_SYMBOL_S + x for x in tags_s])))

def _maybe_wrap(value, maybeNone, sep=None):
    if maybeNone is None:
        return (value, False)
    return (value + sep + maybeNone.pprint(), True)

def _wrap_colon(value, newline=False):
    tail = ""
    if newline:
        tail = "\n"

    return "{}:{}".format(value, tail)

def _wrap_end(value, newline=False):
    if newline:
        return "{}\n{}\n".format(value, util.END_S)
    else:
        return "{}{}\n".format(value, util.END_S)

def _wrap_statement_type(val, type_str):
    return "{} (::{})\n".format(val, STATEMENT_LOOKUPS[type_str])

def _wrap_var_list(val, the_vars, newline=False):
    head = ""
    if newline:
        head = "\n"
    return "{}{}\t | {} |\n".format(val, head, ", ".join([_wrap_var(x.name) for x in the_vars]))


MODAL_LOOKUPS = {}
STATEMENT_LOOKUPS = {}
REGISTERED_PPRINTS = {float : _wrap_float,
                      int   : _wrap_int}

REGISTERED_CONSTRAINTS = set([util.CONSTRAINT_S])

TYPE_WRAPS = {util.REGEX_S  : _wrap_regex,
              util.STRING_S : _wrap_str}

