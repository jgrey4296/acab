#!/usr/bin/env python3
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = root_logger.getLogger(__name__)

import acab.interfaces.context as CtxInt
import acab.error.semantic as ASErr
from acab.core.config import GET
from acab.core.data.value import Sentence
from acab.core.data.instruction import (ProductionComponent,
                                                        ProductionOperator)
from acab import types as AT
config = GET()
CONSTRAINT    = config.prepare("Value.Structure", "CONSTRAINT")()
TYPE_INSTANCE = config.prepare("Value.Structure", "TYPE_INSTANCE")()
ATOM          = config.prepare("Data", "TYPE_BOTTOM_NAME")()


TYPE_OP_SEN = Sentence.build(["τ="])

default_sieve = [
    lambda x: (False, "alpha", [test for test in x.data[CONSTRAINT] if not test.has_var]) if CONSTRAINT in x.data else None,
    lambda x: (False, "beta", [test for test in x.data[CONSTRAINT] if test.has_var]) if CONSTRAINT in x.data else None,
    lambda x: (False, "name", [x]) if x.is_var else None,
    lambda x: (False, "sub_struct_tests", [ProductionComponent(value=TYPE_OP_SEN, params=[x.type])]) if str(x.type) != ATOM else None,
    lambda x: (False, "sub_struct_binds", [("type", x.type)]) if x.type.has_var else None,
]



def make_alpha(x:AT.Value) -> Optional[Tuple[bool, str, List[Any]]]:
    """ A more explicit version of the lambda above
    Take a query word, return any alpha tests (ie: without variables) it has
    """
    if CONSTRAINT not in x.data:
        return None

    return (False, "alpha", [test for test in x.data[CONSTRAINT] if not test.has_var])

# default_sieve.append(make_alpha)
