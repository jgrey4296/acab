"""
Utilities for printing out information
"""
from re import Pattern
from collections import defaultdict
from acab.config import AcabConfig

util = AcabConfig.Get()

CONSTRAINT_S = util("Parsing.Structure", "CONSTRAINT_S")
OPERATOR_S = util("Parsing.Structure", "OPERATOR_S")
QUERY_S = util("Parsing.Structure", "QUERY_S")
NEGATION_S = util("Parsing.Structure", "NEGATION_S")
FALLBACK_S = util("Parsing.Structure", "FALLBACK_S")
VALUE_TYPE_S = util("Parsing.Structure", "VALUE_TYPE_S")

FUNC_SYMBOL_S = util("Printing", "FUNC_SYMBOL_S")

VAR_SYMBOL_S = util("Parsing", "VAR_SYMBOL_S")
AT_VAR_SYMBOL_S = util("Parsing", "AT_VAR_SYMBOL_S")
QUERY_SYMBOL_S = util("Parsing", "QUERY_SYMBOL_S")
NEGATION_SYMBOL_S = util("Parsing", "NEGATION_SYMBOL_S")
TAG_SYMBOL_S = util("Parsing", "TAG_SYMBOL_S")
END_S = util("Parsing", "END_S")

TAB_S = util("Printing", "TAB_S", action=AcabConfig.actions_e.STRIPQUOTE)
FALLBACK_MODAL_S = util("Printing", "FALLBACK_MODAL_S", action=AcabConfig.actions_e.STRIPQUOTE)

#Setup
# TODO register additional constraints
def register_modal(a_dict, reset=False):
    """ Register how to print a modal operator
    ie: For exclusion logic: DOT -> ".", EX -> "!"
    """
    global MODAL_LOOKUPS
    if reset:
        MODAL_LOOKUPS.clear()
    assert(not any([k in MODAL_LOOKUPS for k in a_dict.keys()]))
    MODAL_LOOKUPS.update(a_dict)

def register_primitive(a_dict, reset=False):
    """
    Register additional primitive formats

    The two base primitive examples are:
    REGEX: /{}/
    STRING: "{}"
    """
    global TYPE_WRAPS
    if reset:
        TYPE_WRAPS.clear()
    assert(not any([k in TYPE_WRAPS for k in a_dict.keys()])), a_dict
    assert(all([callable(x) for x in a_dict.values()]))
    TYPE_WRAPS.update(a_dict)

def register_constraint(*constraints, reset=False):
    """
    Register constraints / annotations of a value to represent for a value:
    eg: x (::a.type), or y(>2)...
    """
    global REGISTERED_CONSTRAINTS
    if reset:
        REGISTERED_CONSTRAINTS = set([CONSTRAINTS_S])

    REGISTERED_CONSTRAINTS.update(constraints)

def register_class(cls, func):
    """
    Register a function which prints a specific class

    Enables separating print logic from the class
    """
    global REGISTERED_PPRINTS
    assert(isinstance(cls, type))
    assert(callable(func))
    assert(cls not in REGISTERED_PPRINTS)
    REGISTERED_PPRINTS[cls] = func

def register_obvious_types(*type_instances):
    """ Register Type Instances which do not need pretty printing
    eg: ATOMs and INTs don't need it: an_atom(::atom) 2(::number.int)
    """
    global OBVIOUS_TYPES
    OBVIOUS_TYPES.update(type_instances)

def default_opts(*args, **kwargs):
    """ Create the default options for pprint """
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

    val = pprint(value.name, opts)

    # transform the value format if necessary
    # eg: 2.34 => 2d34
    if hasattr(value, "type") and value.type in TYPE_WRAPS:
        val = TYPE_WRAPS[value.type](val)

    # Wrap binding
    if value.is_var:
        val = _wrap_var(val, is_at_var=value.is_at_var)

    # Wrap constraints
    val = _wrap_constraints(val, value._data)

    # Wrap modal Operator
    print_modal = opts['modal']
    if not print_modal:
        return val

    if OPERATOR_S in value._data:
        return _wrap_modal_operator(val, value._data[OPERATOR_S])

    return _wrap_modal_operator(val, None)

def print_sequence(seq, opts):
    """ Pretty Print a Sequence, ie: things like sentences """
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
    if QUERY_S in seq._data:
        val = _wrap_question(val)

    # Wrap as negated
    if NEGATION_S in seq._data and seq._data[NEGATION_S]:
        val = _wrap_negation(val)

    # Wrap fallback vars
    if FALLBACK_S in seq._data and bool(seq._data[FALLBACK_S]):
        val = _wrap_fallback(val, seq._data[FALLBACK_S])

    return val

def print_container(container, opts):
    the_clauses = [x.pprint(opts) for x in container.clauses]

    join_str = opts['container_join']
    if not join_str:
        join_str = "\n" + TAB_S

    return "{}\n".format(join_str.join(the_clauses))

def print_statement(statement, opts):

    head, body = statement.pprint_has_content

    val = statement._name

    val = _wrap_colon(val)

    val += " "
    val = _wrap_constraints(val, statement._data)
    val += "\n"

    if bool(statement._params):
        val = _wrap_var_list(val, statement._params)

    if bool(statement._tags):
        val = _wrap_tags(val, statement._tags)

    # Add the statement body, which is specific to each statement type
    if head or body:
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
    op_str = "{}{}".format(FUNC_SYMBOL_S, pprint(operator.op))

    join_str = opts['join']
    if not join_str:
        join_str = " "

    the_params = [x.pprint() for x in operator._params]

    head = []
    tail = the_params
    if operator._sugared:
        head = [str(x) for x in the_params[:1]]
        tail = [str(x) for x in the_params[1:]]

    val = join_str.join(head + [op_str] + tail)

    return val

def print_operator_rebind(operator, opts):
    val = print_operator(operator, opts)
    val = _wrap_rebind(val, operator._rebind)
    return val

def print_operator_wrap(operator, opts):
    # Format the vars of the operator
    join_str = opts['join']
    if not join_str:
        join_str = " "

    # TODO handle sugared
    the_params = [x.pprint(opts) for x in operator._params]
    val = "{}{} {}".format(FUNC_SYMBOL_S, pprint(operator.op, opts), join_str.join(the_params))

    return val


# PRINTING COMPONENTS
def _wrap_str(value, opts=None):
    assert(isinstance(value, str))
    return '"{}"'.format(value)

def _wrap_regex(value, opts=None):
    assert(isinstance(value, Pattern))
    val = "/{}/".format(value.pattern)
    return val


def _wrap_var(value, is_at_var=False):
    assert(isinstance(value, str))
    sym = VAR_SYMBOL_S
    if is_at_var:
        sym = AT_VAR_SYMBOL_S
    return sym + value

def _wrap_constraints(value, data):
    assert(isinstance(value, str))
    assert(isinstance(data, dict))

    constraints = []

    if data[VALUE_TYPE_S] not in OBVIOUS_TYPES:
        constraints.append(data[VALUE_TYPE_S])

    # Get registered data annotations:
    for x in REGISTERED_CONSTRAINTS:
        if x in data:
            if isinstance(data[x], list):
                constraints += data[x]
            else:
                constraints.append(data[x])

    result = value
    # Print the constraints
    if bool(constraints):
        cons_strs = ", ".join([pprint(x) for x in constraints])
        result += "({})".format(cons_strs)
    return result

def _wrap_modal_operator(value, op):
    assert(isinstance(value, str)), value
    if op is None:
        return value + FALLBACK_MODAL_S

    assert(op in MODAL_LOOKUPS), op
    return value + MODAL_LOOKUPS[op]

def _wrap_rebind(value, rebind, is_sugar=False):
    arrow = "->"
    if rebind is None:
        return value
    if is_sugar:
        arrow = "=>"


    return "{} {} {}".format(value,
                             arrow,
                             pprint(rebind))

def _wrap_question(value):
    return "{}{}".format(value, QUERY_SYMBOL_S)

def _wrap_negation(value):
    return "{}{}".format(NEGATION_SYMBOL_S, value)

def _wrap_fallback(value, fallback_list):
    return "{} || {}".format(value, print_fallback(fallback_list))

def _wrap_tags(value, tags, sep=TAB_S):
    tags_s = [str(x) for x in tags]
    return "{}{}{}\n\n".format(value, sep, ", ".join(sorted([TAG_SYMBOL_S + x for x in tags_s])))

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
        return "{}\n{}\n".format(value, END_S)
    else:
        return "{}{}\n".format(value, END_S)

def _wrap_var_list(val, the_vars, newline=False):
    head = ""
    if newline:
        head = "\n"
    return "{}{}{}| {} |\n".format(val, head, TAB_S, ", ".join([_wrap_var(x.name) for x in the_vars]))


MODAL_LOOKUPS = {}
REGISTERED_PPRINTS = {Pattern: _wrap_regex}

REGISTERED_CONSTRAINTS = set([CONSTRAINT_S])

TYPE_WRAPS = {}

OBVIOUS_TYPES = set([])
