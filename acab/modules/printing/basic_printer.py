from dataclasses import dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.core.config.config import ConfigSpec
from acab.core.value import instruction as PA
from acab.interfaces.printing import PrintSystem_i
from acab.interfaces import handler_system as HS
from acab.interfaces.value import (Sentence_i, Value_i)
from acab.core.value.instruction import Instruction
import acab.core.defaults.value_keys as DS
from acab.core.config.config import AcabConfig
from acab.core.printing import basic
from acab.core.defaults import print_signals as DSig


config = AcabConfig()


class BasicPrinter(basic.PrintSystemImpl, PrintSystem_i):
    """ Handles how to convert values and sentences into strings,
    does not rely on the underlying data structures
    """
    _default_sieve       : ClassVar[list[Callable]] = [
        # override tuple : 1 -> 1 : any
        lambda x              : x.signal if isinstance(x, HS.HandlerOverride) else None,
        # symbol         : m -> m : any
        lambda x              : DSig.SYMBOL if isinstance(x, ConfigSpec) else None,
        # enum
        lambda x              : DSig.SYMBOL if isinstance(x, Enum) else None,
        # SEM HINT Handler
        lambda x              : str(x.data[DS.SEMANTIC_HINT]) if isinstance(x, Value_i) and DS.SEMANTIC_HINT in x.data else None,
        # exact type     : 1 -> 1 : any / leaf
        lambda x              : str(x.type) if isinstance(x, Value_i) else None,
        # gen type       : m -> 1 : any / leaf
        # structure      : m -> m : leaf
        lambda x              : DSig.STRUCTURE if isinstance(x, PA.ProductionStructure) else None,
        # container      : m -> m : leaf
        lambda x              : DSig.CONTAINER if isinstance(x, PA.ProductionContainer) else None,
        # component      : m -> m : leaf
        lambda x              : DSig.COMPONENT if isinstance(x, PA.ProductionComponent) else None,
        # Statement
        lambda x              : DSig.STATEMENT if isinstance(x, Instruction) else None,
        # sentence       : m -> 1 : any / leaf
        lambda x              : DSig.SENTENCE if isinstance(x, Sentence_i) else None,
        # value          : m -> 1 : any
        lambda x              : DSig.ATOM if isinstance(x, Value_i) else None
    ]

