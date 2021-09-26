import abc
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.abstract.config.config import AcabConfig, ConfigSpec
from acab.abstract.core import production_abstractions as PA
from acab.abstract.core.values import AcabStatement
from acab.abstract.interfaces.handler_system import (Handler,
                                                     HandlerComponent_i,
                                                     HandlerSystem_i)
from acab.abstract.interfaces.value import Sentence_i, Value_i
from acab.abstract.printing.default_symbols import PRINT_SEPARATOR_P
from acab.error.acab_print_exception import AcabPrintException
from acab.error.acab_semantic_exception import AcabSemanticException

logging = root_logger.getLogger(__name__)


Sentence         = 'Sentence'
ModuleComponents = "ModuleComponents"

@dataclass
class PrintSystem_i(HandlerSystem_i):
    """ Handles how to convert values and sentences into strings,
    does not rely on the underlying data structures,
    just consumes Sentences
    """
    separator : ConfigSpec     = field(default=PRINT_SEPARATOR_P)
    settings  : Dict[str, str] = field(default_factory=dict)
    _config   : AcabConfig     = field(init=False, default_factory=AcabConfig.Get)

    def __post_init__(self, handlers):
        super().__post_init__(handlers)
        if self.default is None:
            self.default = Handler("_:default", lambda x, data=None: str(x))

    def check(self, val) -> Optional[str]:
        """ Check a value to toggle variations/get defaults"""
        if val in self.settings:
            return self.settings[val]

        return None

    def pprint(self, *args) -> str:
        """
        The Core Pretty Printer.
        Process a Stack, looking up specific handlers for the top,
        building up a final string

        Handler's return a list of values to go onto stack.
        """
        remaining = [[x, self.separator] for x in args[:-1]] + [args[-1]]
        result = ""
        while bool(remaining):
            current = remaining.pop(0)
            handler = None
            data    = {}
            if isinstance(current, str):
                result += current
                continue
            elif isinstance(current, list):
                remaining = current + remaining
                continue
            else:
                handler, _ = self.lookup(current)

            if isinstance(current, PrintSystem_i.HandlerOverride):
                data.update(current.data)
                current = current.value

            if isinstance(handler, PrintSemantics_i):
                handled = handler(current, top=self, data=data)
            else:
                handled = handler(current, data=data)

            # Add the results of a handler to the head
            if isinstance(handled, list):
                remaining = handled + remaining
            else:
                remaining = [handled] + remaining


        return result


    def __call__(self, *args) -> str:
        return self.pprint(*args)

    def extend(self, mods:List[ModuleComponents]):
        logging.info("Extending Printer")
        for printer in [y for x in mods for y in x.printers]:
            self._register_handler(printer)


#--------------------
@dataclass
class PrintSemantics_i(HandlerComponent_i):

    transforms  : List[Callable] = field(init=False, default_factory=list)

    def __post_init__(self):
        self.transforms += self.add_transforms()

    def add_transforms(self) -> List[Callable]:
        """ Override to add custom transforms in a class """
        return []

    def run_transforms(self, value: Value_i, curr_str: List[Any]) -> List[Any]:
        curr = curr_str
        for trans in self.transforms:
            curr = trans(self, value, curr)

        return curr

    @abc.abstractmethod
    def __call__(self, to_print: Value_i, top:'PrintSystem_i'=None, data=None) -> List[Tuple[str,Value_i]]:
        pass
