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

    return [PC.REGEX_WRAP_P] + current + [PC.REGEX_WRAP_P]


def _maybe_wrap_var(PS, value, current):
    assert(isinstance(current, str))
    if not value.is_var:
        return current

    sym = PC.BIND_P
    if value.is_at_var:
        sym = PC.AT_BIND_P

    return [sym] + current


def _maybe_wrap_modals(PS, value, current):
    """ Add any defined modalities to the end of the string """
    # TODO switch this to a prepare
    use_modals = config.value("MODAL", as_list=True)
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

    if value.data[PC.TYPE_INSTANCE_V] not in PC.OBVIOUS_TYPES_P:
        # TODO lookup this:
        # constraints.append("::")
        constraints.append(value.data[PC.TYPE_INSTANCE_V])

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
    if PC.QUERY_V not in value.data or not value.data[PC.QUERY_V]:
        return current

    return current + [PC.QUERY_SYM]

def _maybe_wrap_negation(PS, value, current):
    if PC.NEGATION_V not in value.data or not value.data[PC.NEGATION_V]:
        return current

    return [PC.NEGATION_SYM] + current

def _wrap_fallback(PS, value, current):
    if PC.QUERY_FALLBACK_V not in value.data:
        return current

    return current + [PC.FALLBACK_SYM] + value.data[PC.QUERY_FALLBACK_V]

def _wrap_tags(PS, value, current):
    if PC.TAG_V not in value.data:
        return current

    tags = zip([PC.TAG_SYM]*len(value.data[PC.TAG_V]),
               value.data[PC.TAG_V])

    return current + tags

def _wrap_var_list(PS, value, current):
    if PC.PARAMS_V not in value:
        return current

    return current + [PC.PARAM_WRAP] + value.data[PC.PARAMS_V] + [PC.PARAM_WRAP]


