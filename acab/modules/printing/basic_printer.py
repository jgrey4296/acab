from dataclasses import dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.core.config.config import ConfigSpec
from acab.core.data import production_abstractions as PA
from acab.interfaces.printing import PrintSystem_i
from acab.interfaces.value import (Sentence_i, Value_i)
from acab.core.data.values import AcabStatement
from acab.core.data.default_structure import SEMANTIC_HINT


class BasicPrinter(PrintSystem_i):
    """ Handles how to convert values and sentences into strings,
    does not rely on the underlying data structures
    """
    _default_sieve       : ClassVar[List[Callable]] = [
        # override tuple : 1 -> 1 : any
        lambda x              : x.signal if isinstance(x, PrintSystem_i.HandlerOverride) else None,
        # symbol         : m -> m : any
        lambda x              : "_:SYMBOL" if isinstance(x, ConfigSpec) else None,
        # enum
        lambda x              : "_:SYMBOL" if isinstance(x, Enum) else None,
        # SEM HINT Handler
        lambda x              : x.data[SEMANTIC_HINT] if isinstance(x, Value_i) and SEMANTIC_HINT in x.data else None,
        # exact type     : 1 -> 1 : any / leaf
        lambda x              : str(x.type) if isinstance(x, Value_i) else None,
        # gen type       : m -> 1 : any / leaf
        # structure      : m -> m : leaf
        lambda x              : "_:STRUCTURE" if isinstance(x, PA.ProductionStructure) else None,
        # container      : m -> m : leaf
        lambda x              : "_:CONTAINER" if isinstance(x, PA.ProductionContainer) else None,
        # component      : m -> m : leaf
        lambda x              : "_:COMPONENT" if isinstance(x, PA.ProductionComponent) else None,
        # Statement
        lambda x              : "_:STATEMENT" if isinstance(x, AcabStatement) else None,
        # sentence       : m -> 1 : any / leaf
        lambda x              : "_:SENTENCE" if isinstance(x, Sentence_i) else None,
        # value          : m -> 1 : any
        lambda x              : "_:ATOM" if isinstance(x, Value_i) else None
    ]

    def __repr__(self):
        return f"({self.__class__.__name__}: {len(self.handler_specs)} handlers, {len(self.sieve)} sieves)"
