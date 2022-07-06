from __future__ import annotations

import logging as logmod
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Protocol,
                    Sequence, Set, Tuple, TypeVar, Union, cast)


import acab.core.defaults.value_keys as DS
import acab.error.semantic as ASErr
import acab.interfaces.context as CtxInt
from acab import AcabConfig
from acab import types as AT
from acab.core.util.sentences import ProductionComponent
from acab.core.value.instruction import ProductionOperator
from acab.interfaces.bind import Bind_i
from acab.interfaces.sieve import AcabSieve
from acab.modules.context.constraint_sieve import default_sieve

if TYPE_CHECKING:
    CtxIns      = 'ContextInstance'
    Constraints = 'ConstraintCollection'
    Operator    = 'ProductionOperator'
    Value       = AT.Value
    Statement   = AT.Instruction
    Sen         = AT.Sentence
    Node        = AT.StructView

logging       = logmod.getLogger(__name__)
config        = AcabConfig()
Bind          = config.prepare("Imports.Targeted", "bind", actions=[config.actions_e.IMCLASS], args={"interface": Bind_i})()
ATOM          = config.attr.Data.TYPE_BASE

CONSTRAINT    = DS.CONSTRAINT
TYPE_INSTANCE = DS.TYPE_INSTANCE

class ConstraintMeta(type(Protocol)):
    """
    Prepares data for the ConstraintCollection, running a sieve on the passed in
    word to group tests togther
    """
    def __init__(cls, name:str, bases:tuple[type, ...], data:dict[str,Any]):
        super(ConstraintMeta, cls).__init__(name, bases, data)

    def __call__(cls, word, *, operators:Mapping=None, sieve_fns:list[Callable]=None):
        if operators is not None:
            cls.operators = operators

        if sieve_fns is None:
            sieve = cls.sieve
        else:
            sieve = AcabSieve(seive_fns)

        tests : dict[str, list[Sen]] = {}
        for result in sieve.fifo_collect(word):
            stop, x, y = result
            if x not in tests:
                tests[x] = []

            tests[x] += y
            if stop:
                break


        return super(ConstraintMeta, cls).__call__(word, tests)
