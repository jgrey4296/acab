"""
Utility functions for building up printed representations of values

"""
from __future__ import annotations

import abc
import re
from dataclasses import InitVar, dataclass, field
from re import Pattern
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

import acab.core.defaults.value_keys as DS
from acab.core.config.config import AcabConfig
from acab.core.defaults import print_symbols as DSYM
from acab.core.value.sentence import Sentence
from acab.interfaces.value import ValueFactory as VF

if TYPE_CHECKING:
    # tc only imports
    pass

config = AcabConfig()

STRING_SEN = VF.sen([DS.STRING_PRIM])
REGEX_SEN  = VF.sen([DS.REGEX_PRIM])

all_modals = config.prepare("MODAL", _type=list)

def _maybe_wrap_str(PS, value, current) -> str:
    if value.type != STRING_SEN:
        return current

    output = [DSYM.STR_WRAP_P] + current + [DSYM.STR_WRAP_P]
    return output


def _maybe_wrap_regex(PS, value, current) -> str:
    if not isinstance(value.value, Pattern) or value.type != REGEX_SEN:
        return current

    return [DSYM.REGEX_WRAP_P] + current + [DSYM.REGEX_WRAP_P]


def _maybe_wrap_var(PS, value, current) -> str:
    if not value.is_var:
        return current

    sym = DSYM.BIND_SYM
    if value.is_at_var:
        sym = DSYM.AT_BIND_SYM

    return [sym] + current


def _maybe_wrap_modals(PS, value, current) -> str:
    """ Add any defined modalities to the end of the string """
    use_modals = PS.check(all_modals)
    modals = []
    for m in use_modals:
        if m in value.data:
            modals.append(value.data[modal])

    return current + modals

def _focus_wrap_modal(PS, value, current) -> str:
    """ Add a *specific* modality to the string, or its default """
    use_modal = PS.check("MODAL")
    if not use_modal:
        return current

    if use_modal in value.data:
        symbol = value.data[use_modal]

    return current + [symbol]

def _maybe_wrap_rebind(PS, value, current) -> str:
    if value.rebind is None:
        return current

    return current + [DSYM.REBIND_SYM, value.rebind]

def _maybe_wrap_question(PS, value, current) -> str:
    if DSYM.QUERY_V not in value.data or not value.data[DS.QUERY]:
        return current

    return current + [DSYM.QUERY_SYM]

def _maybe_wrap_negation(PS, value, current) -> str:
    if DS.NEGATION not in value.data or not value.data[DS.NEGATION]:
        return current

    return [DSYM.NEGATION_SYM] + current

def _wrap_fallback(PS, value, current) -> str:
    if DS.QUERY_FALLBACK not in value.data:
        return current

    return current + [DSYM.FALLBACK_SYM] + value.data[DS.QUERY_FALLBACK]

def _wrap_tags(PS, value, current) -> str:
    if not bool(value.tags):
        return current

    tags = zip([DSYM.TAG_SYM] * len(value.tags),
               value.tags)

    return current + list(tags)


def _wrap_var_list(PS, value, current) -> str:
    if DS.PARAMS not in value:
        return current

    return (current
            + [DSYM.PARAM_WRAP]
            + value.data[DS.PARAMS]
            + [DSYM.PARAM_WRAP, DSYM.CONTAINER_JOIN_P])


def _sep_list(PS, value, current, *, sep=" ") -> str:
    """ given a list, add separators """
    ret_list = []
    if bool(current):
        for x in current:
            ret_list.append(x)
            ret_list.append(sep)

        ret_list.pop()
    return ret_list

def _suppress_modal(PS, value) -> str:
    """ Wrap the list with a meta instruction to ignore modals"""
    return PS.override(False, value, data={"no_modal": True})
