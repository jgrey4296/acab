from dataclasses import dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.abstract.config.config import ConfigSpec
from acab.abstract.core import production_abstractions as PA
from acab.abstract.interfaces.printing_interfaces import PrintSystem
from acab.abstract.interfaces.value_interfaces import (SentenceInterface,
                                                       ValueInterface)
from acab.abstract.core.values import AcabStatement


class BasicPrinter(PrintSystem):
    """ Handles how to convert values and sentences into strings,
    does not rely on the underlying data structures
    """
    _default_sieve       : ClassVar[List[Callable]] = [
        # override tuple : 1 -> 1 : any
        lambda x              : x.override if isinstance(x, PrintSystem.HandlerOverride) else None,
        # symbol         : m -> m : any
        lambda x              : "_:SYMBOL" if isinstance(x, ConfigSpec) else None,
        # enum
        lambda x              : "_:SYMBOL" if isinstance(x, Enum) else None,
        # exact type     : 1 -> 1 : any / leaf
        lambda x              : str(x.type) if isinstance(x, ValueInterface) else None,
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
        lambda x              : "_:SENTENCE" if isinstance(x, SentenceInterface) else None,
        # value          : m -> 1 : any
        lambda x              : "_:ATOM" if isinstance(x, ValueInterface) else None
    ]
