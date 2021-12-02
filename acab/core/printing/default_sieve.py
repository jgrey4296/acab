#!/usr/bin/env python3
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic


# Default Highest Priority Sieve tests
default_sieve_top       : List[Callable] = [
    # override tuple : 1 -> 1 : any
    lambda x              : x.signal if isinstance(x, PrintSystem_i.HandlerOverride) else None,
    # symbol         : m -> m : any
    lambda x              : "SYMBOL" if isinstance(x, ConfigSpec) else None,
    # enum
    lambda x              : "SYMBOL" if isinstance(x, Enum) else None,
    # exact type     : 1 -> 1 : any / leaf
    lambda x              : str(x.type) if isinstance(x, Value_i) else None,
    ]

# Default Lowest Priority Sieve Tests
default_sieve_bottom   : List[Callable] = [
    # structure      : m -> m : leaf
    lambda x              : "STRUCTURE" if isinstance(x, PA.ProductionStructure) else None,
    # container      : m -> m : leaf
    lambda x              : "CONTAINER" if isinstance(x, PA.ProductionContainer) else None,
    # component      : m -> m : leaf
    lambda x              : "COMPONENT" if isinstance(x, PA.ProductionComponent) else None,
    # Statement
    lambda x              : "STATEMENT" if isinstance(x, AcabStatement) else None,
    # sentence       : m -> 1 : any / leaf
    lambda x              : "SENTENCE" if isinstance(x, Sentence_i) else None,
    # value          : m -> 1 : any
    lambda x              : "ATOM" if isinstance(x, Value_i) else None
]
