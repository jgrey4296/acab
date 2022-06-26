#!/usr/bin/env python3
"""
Sieve functions to create entries in a constraint collection.
Uses Rete terminology (alpha, beta)
plus:
name             : checks a binding matches a value
sub_struct_tests : tests internal structure of a value
sub_struct_binds : binds internal structure of a value
"""
from __future__ import annotations

import logging as logmod
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

import acab.error.semantic as ASErr
import acab.interfaces.context as CtxInt
from acab import AcabConfig
from acab import types as AT
from acab.core.defaults import value_keys as DS
from acab.core.defaults.value_keys import CONSTRAINT, OPERATOR, TYPE_INSTANCE
from acab.core.util.sentences import ProductionComponent
from acab.core.value.instruction import ProductionOperator
from acab.interfaces.value import ValueFactory as VF

config        = AcabConfig()
ATOM          = "_:" + config.prepare("Data", "TYPE_BASE")()


TYPE_OP_SEN = VF.sen(["τ="], data={TYPE_INSTANCE: OPERATOR})

default_sieve = [
    lambda x: (False, "alpha", [test for test in x.data[CONSTRAINT] if not test.has_var]) if CONSTRAINT in x.data else None,
    lambda x: (False, "beta", [test for test in x.data[CONSTRAINT] if test.has_var]) if CONSTRAINT in x.data else None,
    lambda x: (False, "name", [x]) if x.is_var else None,
    lambda x: (False, "sub_struct_tests", [ProductionComponent(TYPE_OP_SEN, params=[x.type])]) if x.type != ATOM else None,
    lambda x: (False, "sub_struct_binds", [(DS.TYPE_INSTANCE, x.type)]) if x.type.has_var else None,
]



def make_alpha(x:AT.Value) -> None|Tuple[bool, str, list[Any]]:
    """ A more explicit version of the lambda above
    Take a query word, return any alpha tests (ie: without variables) it has
    """
    if CONSTRAINT not in x.data:
        return None

    return (False, "alpha", [test for test in x.data[CONSTRAINT] if not test.has_var])
