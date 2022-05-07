#!/usr/bin/env python3
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
from acab.core.value.default_structure import (CONSTRAINT, OPERATOR,
                                               TYPE_INSTANCE)
from acab.core.value.instruction import ProductionComponent, ProductionOperator
from acab.core.value.sentence import Sentence
from acab.interfaces.value import ValueFactory_i as VF

config        = AcabConfig()
ATOM          = config.prepare("Data", "TYPE_BASE")()


TYPE_OP_SEN = VF.sen(["τ="], data={TYPE_INSTANCE: OPERATOR})

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
