#!/usr/bin/env python3
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = root_logger.getLogger(__name__)

import acab.error.semantic as ASErr
import acab.interfaces.context as CtxInt
from acab import types as AT
from acab.core.config.config import GET
from acab.core.data.instruction import ProductionComponent, ProductionOperator
from acab.core.data.sentence import Sentence
from acab.interfaces.value import ValueFactory_i as VF

config = GET()
CONSTRAINT    = config.prepare("Value.Structure", "CONSTRAINT")()
TYPE_INSTANCE = config.prepare("Value.Structure", "TYPE_INSTANCE")()
ATOM          = config.prepare("Data", "TYPE_BOTTOM_NAME")()


TYPE_OP_SEN = VF.sen(["τ="])

default_sieve = [
    lambda x: (False, "alpha", [test for test in x.data[CONSTRAINT] if not test.has_var]) if CONSTRAINT in x.data else None,
    lambda x: (False, "beta", [test for test in x.data[CONSTRAINT] if test.has_var]) if CONSTRAINT in x.data else None,
    lambda x: (False, "name", [x]) if x.is_var else None,
    lambda x: (False, "sub_struct_tests", [ProductionComponent(value=TYPE_OP_SEN, params=[x.type])]) if str(x.type) != ATOM else None,
    lambda x: (False, "sub_struct_binds", [("type", x.type)]) if x.type.has_var else None,
]



def make_alpha(x:AT.Value) -> None|Tuple[bool, str, list[Any]]:
    """ A more explicit version of the lambda above
    Take a query word, return any alpha tests (ie: without variables) it has
    """
    if CONSTRAINT not in x.data:
        return None

    return (False, "alpha", [test for test in x.data[CONSTRAINT] if not test.has_var])

# default_sieve.append(make_alpha)
