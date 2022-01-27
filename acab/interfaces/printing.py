import abc
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab import GET
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.interfaces.handler_system import (Handler,
                                            HandlerComponent_i,
                                            HandlerSystem_i,
                                            Handler_Fragment)
from acab.interfaces.value import Sentence_i, Value_i
from acab.core.printing.default_symbols import PRINT_SEPARATOR_P
from acab.error.printing import AcabPrintException
from acab.error.semantic import AcabSemanticException

logging = root_logger.getLogger(__name__)
config = GET()

Sentence         = AT.Sentence
ModuleComponents = AT.ModuleComponents
ConfigSpec       = AT.ConfigSpec

DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()

@dataclass
class PrintSystem_i(HandlerSystem_i):
    """ Handles how to convert values and sentences into strings,
    does not rely on the underlying data structures,
    just consumes Sentences
    """
    separator : ConfigSpec     = field(default=PRINT_SEPARATOR_P)
    settings  : Dict[str, str] = field(default_factory=dict)
    _config   : AcabConfig     = field(init=False, default_factory=AcabConfig.Get)

    def __post_init__(self, specs, handlers, sieve_fns):
        super().__post_init__(specs, handlers, sieve_fns)
        if not bool(self.handler_specs[DEFAULT_HANDLER_SIGNAL]):
            default = Handler(DEFAULT_HANDLER_SIGNAL, lambda x, data=None: str(x))
            self.register(default)

    def __call__(self, *args) -> str:
        return self.pprint(*args)

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
            spec    = None
            data    = {}
            if isinstance(current, str):
                result += current
                continue
            elif isinstance(current, list):
                remaining = current + remaining
                continue
            else:
                spec = self.lookup(current)

            if isinstance(current, PrintSystem_i.HandlerOverride):
                data.update(current.data)
                current = current.value

            if spec.check_api(func=PrintSemantics_i):
                logging.debug(f"(Remain:{len(remaining):2}) Calling: {spec} : {current}")
                handled = spec[0](current, top=self, data=data)
            else:
                handled = spec[0](current, data=data)

            if not isinstance(handled, list):
                handled = [handled]

            # Add the results of a handler to the head
            remaining = handled + remaining


        return result


    def extend(self, mods:List[ModuleComponents]):
        logging.info("Extending Printer")
        printers = [y for x in mods for y in x.printers]
        assert(all([isinstance(x, Printer_Fragment) for x in printers]))
        for print_fragment in printers:
            assert(issubclass(print_fragment.target_i, PrintSystem_i))
            for val in print_fragment:
                self.register(val)


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

    def verify(self, instruction) -> bool:
        return True

    @abc.abstractmethod
    def __call__(self, to_print: Value_i, *, top:'PrintSystem_i'=None, data=None) -> List[Union[str,Value_i]]:
        pass

# For Modules #################################################################
@dataclass
class Printer_Fragment(Handler_Fragment):

    target_i : HandlerSystem_i = field(default=PrintSystem_i)
