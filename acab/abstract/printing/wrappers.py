#!/usr/bin/env python
import re
from re import Pattern
from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import Sentence
from acab.abstract.printing import consts as PC

config = AcabConfig.Get()


def _maybe_wrap_str(PS, value, current):
    if value.type != PC.STRING_SEN:
        return current

    output = [PC.STR_WRAP_P] + current + [PC.STR_WRAP_P]
    return output


def _maybe_wrap_regex(PS, value, current):
    if not isinstance(value.value, Pattern) or value.type != PC.REGEX_SEN:
        return current

    # originally /{}/
    reg_wrap = PS.use(PC.REGEX_WRAP_P)
    return reg_wrap.format(value.name)


def _maybe_wrap_var(PS, value, current):
    assert(isinstance(current, str))
    sym = PS.use(PC.BIND_P)
    if value.is_at_var:
        sym = PS.use(PC.AT_BIND_P)
    if value.is_var:
        return sym + current
    else:
        return current


def _maybe_wrap_modals(PS, value, current):
    """ Add any defined modalities to the end of the string """
    # TODO switch this to a prepare
    modals = config.value("MODAL", as_list=True)
    curr = current
    for modal in modals:
        if modal in value.data:
            symbol = PS.use((modal, value.data[modal].name))
            curr += symbol

    return curr

def _focus_wrap_modal(PS, value, current):
    """ Add a *specific* modality to the string, or its default """
    use_modal = PS.check("MODAL")
    if not use_modal:
        return current

    if use_modal in value.data:
        symbol = PS.use((use_modal, value.data[use_modal].name))

    return current + symbol


def _wrap_constraints(PS, value, data):
    assert(isinstance(value, str))
    assert(isinstance(data, dict))

    constraints = []

    if data[PC.TYPE_INSTANCE_V] not in PC.OBVIOUS_TYPES_P:
        constraints.append(data[PC.TYPE_INSTANCE_V])

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

    arrow = PS.use(PC.REBIND_P)
    if is_sugar:
        arrow = PS.use(PC.SUGAR_P)

    return " {} {}".format(arrow, str(rebind))

def _maybe_wrap_question(PS, value, current):
    query_symbol = ""
    if PC.QUERY_V in value.data and value.data[PC.QUERY_V]:
        query_symbol = PS.ask(PC.QUERY_V)

    return "{}{}".format(current, query_symbol)

def _maybe_wrap_negation(PS, value, current):
    neg_symbol = ""
    if PC.NEGATION_V in value.data and value.data[PC.NEGATION_V]:
        neg_symbol = PS.ask(PC.NEGATION_V)

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
        sep = PS.use(PC.TAB_P)
    tags_s = [str(x) for x in tags]
    tag_symbol = PS.use(PC.TAG_P)
    return "{}{}{}\n\n".format(value, sep, ", ".join(sorted([tag_symbol + x for x in tags_s])))

def _wrap_colon(PS, value, newline=False):
    tail = ""
    if newline:
        tail = "\n"

    return "{}:{}".format(value, tail)

def _wrap_end(PS, value, newline=False):
    end_symbol = PS.use(PC.END_P)
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

    wrap_fmt = PS.use(PC.WRAP_FORMAT_P)
    join_fmt = PS.use(PC.PARAM_JOIN_P)

    if wrap is not None:
        wrap_fmt = wrap
    if join is not None:
        join_fmt = join

    return " " + wrap_fmt.format(join_fmt.join(maybe_list))


def _remove_trailing_modal(PS, value, current):
    """ For use in printing sentences """
    symbols = "".join(config.printing_extension.values()).replace(".", "\.")
    symbol_re = re.compile("[{}]+$".format(symbols))
    return symbol_re.sub("", current)
