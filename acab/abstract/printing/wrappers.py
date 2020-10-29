#!/usr/bin/env python
from re import Pattern
from acab.config import AcabConfig


util = AcabConfig.Get()

AT_BIND_S         = util("Parsing.Structure", "AT_BIND_S")
CONSTRAINT_S     = util("Parsing.Structure", "CONSTRAINT_S")
END_S            = util("Parsing.Structure", "END_S")
FALLBACK_MODAL_S = util("Printing", "FALLBACK_MODAL_S", actions=[AcabConfig.actions_e.STRIPQUOTE])
FUNC_S           = util("Parsing.Structure", "FUNC_S")
NEGATION_S       = util("Parsing.Structure", "NEGATION_S")
OBVIOUS_TYPES    = []
OPERATOR_S       = util("Parsing.Structure", "OPERATOR_S")
QUERY_S          = util("Parsing.Structure", "QUERY_S")
TAB_S            = util("Printing", "TAB_S", actions=[AcabConfig.actions_e.STRIPQUOTE])
TAG_S            = util("Parsing.Structure", "TAG_S")
VALUE_TYPE_S     = util("Parsing.Structure", "VALUE_TYPE_S")
BIND_S            = util("Parsing.Structure", "BIND_S")

def _maybe_wrap_str(PS, value, current):
    return '"{}"'.format(current)

def _wrap_regex(PS, value, current):
    if not isinstance(value.value, Pattern):
        return current

    val = "/{}/".format(current)
    return val


def _maybe_wrap_var(PS, value, current):
    assert(isinstance(value, str))
    sym = PS.ask("BIND_")
    if value.is_at_var:
        sym = PS.ask("AT_BIND_")
    if value.is_var:
        return sym + current
    else:
        return current


def _wrap_constraints(value, data):
    assert(isinstance(value, str))
    assert(isinstance(data, dict))

    constraints = []

    if data[VALUE_TYPE_S] not in OBVIOUS_TYPES:
        constraints.append(data[VALUE_TYPE_S])

    # # Get registered data annotations:
    # for x in REGISTERED_CONSTRAINTS:
    #     if x in data:
    #         if isinstance(data[x], list):
    #             constraints += data[x]
    #         else:
    #             constraints.append(data[x])

    result = value
    # Print the constraints
    if bool(constraints):
        cons_strs = ", ".join([str(x) for x in constraints])
        result += "({})".format(cons_strs)
    return result

def _modal_operator(PS, value, current):
    modal_data_field = PS.ask('MODAL_FIELD')
    if modal_data_field not in value._data:
        modal_str = PS.ask("FALLBACK_MODAL_S")
    else:
        modal_str = PS.ask(modal_data_field)

    return modal_str

def _wrap_rebind(PS, value, rebind, is_sugar=False):
    arrow = PS.ask(REBIND_S)
    if rebind is None:
        return value
    if is_sugar:
        arrow = PS.ask(SUGAR_S)

    return "{} {} {}".format(value,
                             arrow,
                             str(rebind))

def _maybe_wrap_question(PS, value, current):
    query_symbol = ""
    if QUERY_S in value._data and value._data[QUERY_S]:
        query_symbol = PS.ask(QUERY_S)

    return "{}{}".format(current, query_symbol)

def _maybe_wrap_negation(PS, value, current):
    neg_symbol = ""
    if NEGATION_S in value._data and value._data[NEGATION_S]:
        neg_symbol = PS.ask(NEGATION_S)

    return "{}{}".format(neg_symbol, current)

def _wrap_fallback(PS, the_list):
    assert(len(the_list)%2 == 0)

    the_vars = [x for i, x in enumerate(the_list) if i%2==0]
    the_vals = [x for i, x in enumerate(the_list) if i%2==1]

    joined = ", ".join(["{}:{}".format(x, y) for x, y
                        in zip(the_vars, the_vals)])

    # TODO shove this into config file
    return " || {}".format(joined)

def _wrap_tags(PS, value, tags, sep=TAB_S):
    tags_s = [str(x) for x in tags]
    tag_symbol = PS.ask(TAG_S)
    return "{}{}{}\n\n".format(value, sep, ", ".join(sorted([tag_symbol + x for x in tags_s])))

def _wrap_colon(PS, value, newline=False):
    tail = ""
    if newline:
        tail = "\n"

    return "{}:{}".format(value, tail)

def _wrap_end(PS, value, newline=False):
    end_symbol = PS.ask(END_S)
    if newline:
        return "{}\n{}\n".format(value, end_symbol)
    else:
        return "{}{}\n".format(value, end_symbol)

def _wrap_var_list(PS, val, current):
    raise NotImplementedError()
    # head = ""
    # if newline:
    #     head = "\n"
    # return "{}{}{}| {} |\n".format(val, head, TAB_S, ", ".join([_maybe_wrap_var(x.name) for x in the_vars]))
