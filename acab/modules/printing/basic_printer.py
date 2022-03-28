from dataclasses import dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.core.config.config import ConfigSpec
from acab.core.data import instruction as PA
from acab.interfaces.printing import PrintSystem_i
from acab.interfaces import handler_system as HS
from acab.interfaces.value import (Sentence_i, Value_i)
from acab.core.data.instruction import Instruction
from acab.core.data.default_structure import SEMANTIC_HINT
from acab.core.config.config import AcabConfig
from acab.core.printing import basic

config = AcabConfig()


class BasicPrinter(basic.PrintSystemImpl, PrintSystem_i):
    """ Handles how to convert values and sentences into strings,
    does not rely on the underlying data structures
    """
    _default_sieve       : ClassVar[list[Callable]] = [
        # override tuple : 1 -> 1 : any
        lambda x              : x.signal if isinstance(x, HS.HandlerOverride) else None,
        # symbol         : m -> m : any
        lambda x              : "SYMBOL" if isinstance(x, ConfigSpec) else None,
        # enum
        lambda x              : "SYMBOL" if isinstance(x, Enum) else None,
        # SEM HINT Handler
        lambda x              : str(x.data[SEMANTIC_HINT]) if isinstance(x, Value_i) and SEMANTIC_HINT in x.data else None,
        # exact type     : 1 -> 1 : any / leaf
        lambda x              : str(x.type) if isinstance(x, Value_i) else None,
        # gen type       : m -> 1 : any / leaf
        # structure      : m -> m : leaf
        lambda x              : "STRUCTURE" if isinstance(x, PA.ProductionStructure) else None,
        # container      : m -> m : leaf
        lambda x              : "CONTAINER" if isinstance(x, PA.ProductionContainer) else None,
        # component      : m -> m : leaf
        lambda x              : "COMPONENT" if isinstance(x, PA.ProductionComponent) else None,
        # Statement
        lambda x              : "STATEMENT" if isinstance(x, Instruction) else None,
        # sentence       : m -> 1 : any / leaf
        lambda x              : "SENTENCE" if isinstance(x, Sentence_i) else None,
        # value          : m -> 1 : any
        lambda x              : "ATOM" if isinstance(x, Value_i) else None
    ]

    def __repr__(self):
        return f"({self.__class__.__name__}: {len(self.handler_specs)} handlers, {len(self.sieve)} sieves)"
