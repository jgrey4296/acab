import abc
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.abstract.interfaces.util as SU
from acab.abstract.config.config import AcabConfig, ConfigSpec
from acab.abstract.core import production_abstractions as PA
from acab.abstract.core.values import AcabStatement
from acab.abstract.interfaces.handler_system_interface import (
    HandlerComponent, HandlerSystemInterface)
from acab.abstract.interfaces.value_interfaces import (SentenceInterface,
                                                       ValueInterface)
from acab.error.acab_print_exception import AcabPrintException
from acab.error.acab_semantic_exception import AcabSemanticException
from acab.modules.semantics.context_container import ContextContainer

logging = root_logger.getLogger(__name__)


Sentence        = 'Sentence'
@dataclass
class PrintSystem(HandlerSystemInterface):
    """ Handles how to convert values and sentences into strings,
    does not rely on the underlying data structures
    """
    settings             : Dict[str, str]           = field(default_factory=dict)
    _config              : AcabConfig               = field(init=False, default_factory=AcabConfig.Get)

    _default_sieve       : ClassVar[List[Callable]] = [
        # override tuple : 1 -> 1 : any
        lambda x              : x.override if isinstance(x, PrintSystem.PrintOverride) else None,
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

    @dataclass
    class PrintOverride:
        """ Simple Wrapped for forced semantic use """
        override : str              = field()
        data     : 'ValueInterface' = field()
    #----------------------------------------



    def __post_init__(self, handlers, structs):
        super().__post_init__(handlers, structs)
        if self.default[0] is None:
            self.default = (lambda x: str(x), None)

    def check(self, val) -> Optional[str]:
        """ Check a value to toggle variations/get defaults"""
        if val in self.settings:
            return self.settings[val]

        return None

    def override(self, new_target: str, value) -> 'PrintOverride':
        if new_target not in self.registered_handlers:
            raise AcabPrintException(f"Undefined override handler: {new_target}")

        return PrintSystem.PrintOverride(new_target, value)


    def pprint(self, *args) -> str:
        # TODO add default join symbol
        remaining = list(args[:])
        result = ""
        while bool(remaining):
            current = remaining.pop(0)
            handler = None
            if isinstance(current, str):
                result += current
            elif isinstance(current, list):
                remaining = current + remaining
            elif isinstance(current, PrintSystem.PrintOverride):
                handler = self.registered_handlers[current.override]
                current = current.data
            else:
                handler, _ = self.lookup(current)

            if handler is None:
                continue

            if isinstance(handler, PrintSemantics):
                handled = handler(current, top=self)
            else:
                handled = handler(current)

            # Add the results of a handler to the head
            if isinstance(handled, list):
                remaining = handled + remaining
            else:
                remaining = [handled] + remaining


        return result


    def __call__(self, *args) -> str:
        return self.pprint(*args)
#--------------------
@dataclass
class PrintSemantics(HandlerComponent):

    transforms  : List[Callable] = field(init=False, default_factory=list)

    def __post_init__(self):
        self.transforms += self.add_transforms()

    def add_transforms(self) -> List[Callable]:
        """ Override to add custom transforms in a class """
        return []

    def run_transforms(self, value: ValueInterface, curr_str: List[Any]) -> List[Any]:
        curr = curr_str
        for trans in self.transforms:
            curr = trans(self, value, curr)

        return curr

    @abc.abstractmethod
    def __call__(self, to_print: ValueInterface, top:'PrintSystem'=None) -> List[Tuple[str,ValueInterface]]:
        pass
