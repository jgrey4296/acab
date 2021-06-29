#!/usr/bin/env python
import re
from re import Pattern
from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import Sentence
from acab.abstract.printing import consts as PC
from acab.abstract.core import default_structure as DS

config = AcabConfig.Get()

STRING_SEN = Sentence.build([DS.STRING_PRIM])
REGEX_SEN  = Sentence.build([DS.REGEX_PRIM])

all_modals = config.prepare("MODAL", as_list=True)

def _maybe_wrap_str(PS, value, current):
    if value.type != STRING_SEN:
        return current

    output = [PC.STR_WRAP_P] + current + [PC.STR_WRAP_P]
    return output


def _maybe_wrap_regex(PS, value, current):
    if not isinstance(value.value, Pattern) or value.type != REGEX_SEN:
        return current

    return [PC.REGEX_WRAP_P] + current + [PC.REGEX_WRAP_P]


def _maybe_wrap_var(PS, value, current):
    if not value.is_var:
        return current

    sym = PC.BIND_SYM
    if value.is_at_var:
        sym = PC.AT_BIND_SYM

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


def _wrap_annotations(PS, value, current):
    annotations = []

    if value.data[DS.TYPE_INSTANCE] not in PC.OBVIOUS_TYPES_P:
        # TODO lookup this:
        # constraints.append("::")
        constraints.append(value.data[DS.TYPE_INSTANCE])

    # Get registered data annotations:
    for x in REGISTERED_CONSTRAINTS:
        if x in value.data:
            if isinstance(value.data[x], list):
                constraints += value.data[x]
            else:
                constraints.append(value.data[x])

    # Print the constraints
    if bool(constraints):
        # TODO lookup this
        # annotations = ["("] + annotations + [")"]
        pass

    return current + annotations

def _maybe_wrap_rebind(PS, value, current):
    if value.rebind is None:
        return current

    return current + [PC.REBIND_SYM, value.rebind]

def _maybe_wrap_question(PS, value, current):
    if PC.QUERY_V not in value.data or not value.data[DS.QUERY]:
        return current

    return current + [PC.QUERY_SYM]

def _maybe_wrap_negation(PS, value, current):
    if DS.NEGATION not in value.data or not value.data[DS.NEGATION]:
        return current

    return [PC.NEGATION_SYM] + current

def _wrap_fallback(PS, value, current):
    if DS.QUERY_FALLBACK not in value.data:
        return current

    return current + [PC.FALLBACK_SYM] + value.data[DS.QUERY_FALLBACK]

def _wrap_tags(PS, value, current):
    if DS.TAG not in value.data:
        return current

    tags = zip([PC.TAG_SYM] * len(value.data[DS.TAG]),
               value.data[DS.TAG])

    return current + tags

def _wrap_var_list(PS, value, current):
    if DS.PARAMS not in value:
        return current

    return current + [PC.PARAM_WRAP] + value.data[DS.PARAMS] + [PC.PARAM_WRAP]


def _sep_list(PS, value, current, sep=" "):
    """ given a list, add separators """
    ret_list = []
    for x in current[:-1]:
        ret_list.append(x)
        ret_list.append(sep)

    ret_list.append(current[-1])
    return ret_list

def _suppress_modal(PS, value, current):
    """ Wrap the list with a meta instruction to ignore modals"""


    return ret_list
