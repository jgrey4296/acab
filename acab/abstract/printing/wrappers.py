#!/usr/bin/env python
from re import Pattern
from acab.abstract.config.config import AcabConfig

util = AcabConfig.Get()
#Static
TYPE_INSTANCE_V  = util.value("Value.Structure", "TYPE_INSTANCE")
BIND_V           = util.value("Value.Structure", "BIND")
CONSTRAINT_V     = util.value("Value.Structure", "CONSTRAINT")
OPERATOR_V       = util.value("Value.Structure", "OPERATOR")
NEGATION_V       = util.value("Value.Structure", "NEGATION")
QUERY_V          = util.value("Value.Structure", "QUERY")

# Runtime dynamic
OBVIOUS_TYPES_P  = util.prepare("Print.Data", "SUPPRESSION_TYPES", actions=[AcabConfig.actions_e.SPLIT])

TAB_P            = util.prepare("Print.Patterns", "TAB", actions=[AcabConfig.actions_e.STRIPQUOTE])
WRAP_FORMAT_P    = util.prepare("Print.Patterns", "WRAP_FORMAT")
PARAM_JOIN_P     = util.prepare("Print.Patterns", "PARAM_JOIN")
STR_WRAP_P       = util.prepare("Print.Patterns", "STR_WRAP")
REGEX_WRAP_P     = util.prepare("Print.Patterns", "REGEX_WRAP")


BIND_P           = util.prepare("Symbols", "BIND")
AT_BIND_P        = util.prepare("Symbols", "AT_BIND")
END_P            = util.prepare("Symbols", "END")
FALLBACK_MODAL_P = util.prepare("Symbols", "FALLBACK_MODAL", actions=[AcabConfig.actions_e.STRIPQUOTE])
FUNC_P           = util.prepare("Symbols", "FUNC")
NEGATION_P       = util.prepare("Symbols", "NEGATION")
QUERY_P          = util.prepare("Symbols", "QUERY")
TAG_P            = util.prepare("Symbols", "TAG")
REBIND_P         = util.prepare("Symbols", "REBIND")
SUGAR_P          = util.prepare("Symbols", "SUGAR")

# TODO add an action to build a sentence
STRING_V           = util.value("Type.Primitive", "STRING")
REGEX_V            = util.value("Type.Primitive", "REGEX")



def _maybe_wrap_str(PS, value, current):
    if value.type != STRING_V:
        return current

    # originally "{}"
    str_wrap = PS.use(STR_WRAP_P)
    return str_wrap.format(current)

def _maybe_wrap_regex(PS, value, current):
    if not isinstance(value.value, Pattern) or value.type != REGEX_V:
        return current

    # originally /{}/
    reg_wrap = PS.use(REGEX_WRAP_P)
    val = reg_wrap.format(value.name)
    return val


def _maybe_wrap_var(PS, value, current):
    assert(isinstance(current, str))
    sym = PS.use(BIND_P)
    if value.is_at_var:
        sym = PS.use(AT_BIND_P)
    if value.is_var:
        return sym + current
    else:
        return current


def _wrap_constraints(PS, value, data):
    assert(isinstance(value, str))
    assert(isinstance(data, dict))

    constraints = []

    if data[TYPE_INSTANCE_V] not in OBVIOUS_TYPES_P:
        constraints.append(data[TYPE_INSTANCE_V])

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

def _maybe_wrap_rebind(PS, rebind, is_sugar=False):
    if rebind is None:
        return ""

    arrow = PS.use(REBIND_P)
    if is_sugar:
        arrow = PS.use(SUGAR_P)

    return " {} {}".format(arrow, str(rebind))

def _maybe_wrap_question(PS, value, current):
    query_symbol = ""
    if QUERY_V in value._data and value._data[QUERY_V]:
        query_symbol = PS.ask(QUERY_V)

    return "{}{}".format(current, query_symbol)

def _maybe_wrap_negation(PS, value, current):
    neg_symbol = ""
    if NEGATION_V in value._data and value._data[NEGATION_V]:
        neg_symbol = PS.ask(NEGATION_V)

    return "{}{}".format(neg_symbol, current)

def _wrap_fallback(PS, the_list):
    assert(len(the_list)%2 == 0)

    the_vars = [x for i, x in enumerate(the_list) if i%2==0]
    the_vals = [x for i, x in enumerate(the_list) if i%2==1]

    joined = ", ".join(["{}:{}".format(x, y) for x, y
                        in zip(the_vars, the_vals)])

    # TODO shove this into config file
    return " || {}".format(joined)

def _wrap_tags(PS, value, tags, sep=None):
    if sep is None:
        sep = PS.use(TAB_P)
    tags_s = [str(x) for x in tags]
    tag_symbol = PS.use(TAG_P)
    return "{}{}{}\n\n".format(value, sep, ", ".join(sorted([tag_symbol + x for x in tags_s])))

def _wrap_colon(PS, value, newline=False):
    tail = ""
    if newline:
        tail = "\n"

    return "{}:{}".format(value, tail)

def _wrap_end(PS, value, newline=False):
    end_symbol = PS.use(END_P)
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


def _maybe_wrap_list(PS, maybe_list, wrap=None, join=None):
    if not bool(maybe_list):
        return ""

    wrap_fmt = PS.use(WRAP_FORMAT_P)
    join_fmt = PS.use(PARAM_JOIN_P)

    if wrap is not None:
        wrap_fmt = wrap
    if join is not None:
        join_fmt = join

    return " " + wrap_fmt.format(join_fmt.join(maybe_list))
