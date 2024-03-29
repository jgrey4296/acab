#!/usr/bin/env python3
##-- imports
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab.interfaces.handler_system import HandlerOverride
import acab.core.values.instruction as PA
import acab.interfaces.value as VI
from acab.interfaces.config import ConfigSpec_d
from acab.core.defaults import print_signals as DS

from enum import Enum

##-- end imports

# Default Highest Priority Sieve tests

default_sieve_top       : list[Callable] = [
    # override tuple : 1 -> 1 : any
    lambda x              : x.signal if isinstance(x, HandlerOverride) else None,
    # symbol         : m -> m : any
    lambda x              : DS.SYMBOL if isinstance(x, ConfigSpec_d) else None,
    # enum
    lambda x              : DS.SYMBOL if isinstance(x, Enum) else None,
    # exact type     : 1 -> 1 : any / leaf
    lambda x              : x.type if isinstance(x, VI.Value_i) else None,
    ]

# Default Lowest Priority Sieve Tests

default_sieve_bottom   : list[Callable] = [
    # structure      : m -> m : leaf
    lambda x              : DS.STRUCTURE if isinstance(x, PA.ProductionStructure) else None,
    # container      : m -> m : leaf
    lambda x              : DS.CONTAINER if isinstance(x, PA.ProductionContainer) else None,
    # component      : m -> m : leaf
    lambda x              : DS.COMPONENT if isinstance(x, PA.ProductionComponent) else None,
    # Statement
    lambda x              : DS.STATEMENT if isinstance(x, VI.Instruction_i) else None,
    # sentence       : m -> 1 : any / leaf
    lambda x              : DS.SENTENCE if isinstance(x, VI.Sentence_i) else None,
    # value          : m -> 1 : any
    lambda x              : DS.ATOM if isinstance(x, VI.Value_i) else None
]
