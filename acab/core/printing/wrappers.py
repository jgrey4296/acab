import re
from re import Pattern
from acab.core.config.config import AcabConfig
from acab.core.data.value import Sentence
from acab.core.printing import default_symbols as DSYM
from acab.core.data import default_structure as DS

config = AcabConfig.Get()

STRING_SEN = Sentence.build([DS.STRING_PRIM])
REGEX_SEN  = Sentence.build([DS.REGEX_PRIM])

all_modals = config.prepare("MODAL", as_list=True)


def _maybe_wrap_str(PS, value, current):
    if value.type != STRING_SEN:
        return current

    output = [DSYM.STR_WRAP_P] + current + [DSYM.STR_WRAP_P]
    return output


def _maybe_wrap_regex(PS, value, current):
    if not isinstance(value.value, Pattern) or value.type != REGEX_SEN:
        return current

    return [DSYM.REGEX_WRAP_P] + current + [DSYM.REGEX_WRAP_P]


def _maybe_wrap_var(PS, value, current):
    if not value.is_var:
        return current

    sym = DSYM.BIND_SYM
    if value.is_at_var:
        sym = DSYM.AT_BIND_SYM

    return [sym] + current


def _maybe_wrap_modals(PS, value, current):
    """ Add any defined modalities to the end of the string """
    use_modals = PS.check(all_modals)
    modals = []
    for m in use_modals:
        if m in value.data:
            modals.append(value.data[modal])

    return current + modals

def _focus_wrap_modal(PS, value, current):
    """ Add a *specific* modality to the string, or its default """
    use_modal = PS.check("MODAL")
    if not use_modal:
        return current

    if use_modal in value.data:
        symbol = value.data[use_modal]

    return current + [symbol]

def _maybe_wrap_rebind(PS, value, current):
    if value.rebind is None:
        return current

    return current + [DSYM.REBIND_SYM, value.rebind]

def _maybe_wrap_question(PS, value, current):
    if DSYM.QUERY_V not in value.data or not value.data[DS.QUERY]:
        return current

    return current + [DSYM.QUERY_SYM]

def _maybe_wrap_negation(PS, value, current):
    if DS.NEGATION not in value.data or not value.data[DS.NEGATION]:
        return current

    return [DSYM.NEGATION_SYM] + current

def _wrap_fallback(PS, value, current):
    if DS.QUERY_FALLBACK not in value.data:
        return current

    return current + [DSYM.FALLBACK_SYM] + value.data[DS.QUERY_FALLBACK]

def _wrap_tags(PS, value, current):
    if not bool(value.tags):
        return current

    tags = zip([DSYM.TAG_SYM] * len(value.tags),
               value.tags)

    return current + list(tags)


def _wrap_var_list(PS, value, current):
    if DS.PARAMS not in value:
        return current

    return (current
            + [DSYM.PARAM_WRAP]
            + value.data[DS.PARAMS]
            + [DSYM.PARAM_WRAP, DSYM.CONTAINER_JOIN_P])


def _sep_list(PS, value, current, *, sep=" "):
    """ given a list, add separators """
    ret_list = []
    if bool(current):
        for x in current:
            ret_list.append(x)
            ret_list.append(sep)

        ret_list.pop()
    return ret_list

def _suppress_modal(PS, value):
    """ Wrap the list with a meta instruction to ignore modals"""
    return PS.override(False, value, data={"no_modal": True})
