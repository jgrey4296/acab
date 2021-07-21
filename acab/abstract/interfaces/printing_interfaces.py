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
from acab.abstract.printing.default_symbols import PRINT_SEPARATOR_P
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
    separator : ConfigSpec     = field(default=PRINT_SEPARATOR_P)
    settings  : Dict[str, str] = field(default_factory=dict)
    _config   : AcabConfig     = field(init=False, default_factory=AcabConfig.Get)

    def __post_init__(self, handlers, structs):
        super().__post_init__(handlers, structs)
        if self.default[0] is None:
            self.default = (lambda x: str(x), None)

    def check(self, val) -> Optional[str]:
        """ Check a value to toggle variations/get defaults"""
        if val in self.settings:
            return self.settings[val]

        return None

    def pprint(self, *args) -> str:
        remaining = [[x, self.separator] for x in args[:-1]] + [args[-1]]
        result = ""
        while bool(remaining):
            current = remaining.pop(0)
            handler = None
            if isinstance(current, str):
                result += current
                continue
            elif isinstance(current, list):
                remaining = current + remaining
                continue
            else:
                handler, _ = self.lookup(current)

            if isinstance(current, PrintSystem.HandlerOverride):
                current = current.data

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
