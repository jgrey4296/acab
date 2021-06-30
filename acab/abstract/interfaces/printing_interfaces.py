import abc
from dataclasses import InitVar, dataclass, field
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.abstract.interfaces.util as SU
from acab.abstract.config.config import AcabConfig, ConfigSpec
from acab.abstract.interfaces.value_interfaces import (SentenceInterface,
                                                       ValueInterface)
from acab.error.acab_print_exception import AcabPrintException
from acab.error.acab_semantic_exception import AcabSemanticException
from acab.modules.semantics.context_container import ContextContainer

Sentence        = 'Sentence'
@dataclass
class PrintSemanticSystem(metaclass=abc.ABCMeta):
    """ Handles how to convert values and sentences into strings,
    does not rely on the underlying data structures
    """

    # Handlers: fn val -> [Val|str]
    handlers        : InitVar[List[Callable]]    = field()
    # Ordered Sieve for lookup of handlers. Most -> least specific
    # fn x -> str
    sieve           : List[Callable]             = field(default_factory=list)
    settings        : Dict[str, str]             = field(default_factory=dict)

    _config    : AcabConfig                      = field(init=False, default_factory=AcabConfig.Get)
    registered_handlers : Dict[str, Callable]    = field(init=False, default_factory=dict)

    _default_sieve : ClassVar[List[Callable]] = [
        # override tuple : 1 -> 1 : any
        lambda x: x.override if isinstance(x, PrintSemanticSystem.PrintOverride) else None,
        # symbol         : m -> m : any
        lambda x: "_:SYMBOL" if isinstance(x, ConfigSpec) else None,
        # enum
        lambda x: "_:SYMBOL" if isinstance(x, Enum) else None,
        # exact type     : 1 -> 1 : any / leaf
        lambda x: str(x.type),
        # gen type       : m -> 1 : any / leaf
        # structure      : m -> m : leaf
        # container      : m -> m : leaf
        # component      : m -> m : leaf
        # sentence       : m -> 1 : any / leaf
        lambda x: "_:SENTENCE" if isinstance(x, SentenceInterface) else None,
        # value          : m -> 1 : any
        lambda x: "_:ATOM" if isinstance(x, ValueInterface) else None
    ]


    @dataclass
    class PrintOverride:
        """ Simple Wrapped for forced semantic use """
        override : str              = field()
        data     : 'ValueInterface' = field()
    #----------------------------------------


    def __post_init__(self, handlers):
        # use default sieve if sieve is empty
        if not bool(self.sieve):
            self.sieve += PrintSemanticSystem._default_sieve
        # register provided handlers
        for handler in handlers:
            self._register_handler(handler)

    def _register_handler(self, handler):
        # TODO maybe handle tuples later
        pair_str = handler.paired_with
        assert(pair_str not in self.registered_handlers)
        self.registered_handlers[pair_str] = handler


    def lookup(self, value: ValueInterface) -> 'PrintSemantics':
        # sieve from most to least specific

        for sieve_fn in self.sieve:
            key = sieve_fn(value)
            if bool(key) and key in self.registered_handlers:
                return self.registered_handlers[key]

        # Final resort
        logging.warning(f"Resorting to str handler for: {value}")
        return lambda x: str(x)

    def check(self, val) -> Optional[str]:
        """ Check a value to toggle variations/get defaults"""
        if val in self.settings:
            return self.settings[val]

        return None

    def override(self, new_target: str, value) -> 'PrintOverride':
        if new_target not in self.registered_handlers:
            raise AcabPrintException(f"Undefined override handler: {new_target}")

        return PrintSemanticSystem.PrintOverride(new_target, value)


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
            elif isinstance(current, PrintSemanticSystem.PrintOverride):
                handler = self.registered_handlers[current.override]
                current = current.data
            else:
                handler = self.lookup(current)

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
@dataclass
class PrintSemantics(metaclass=abc.ABCMeta):

    paired_with : Sentence       = field()
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
    def __call__(self, to_print: ValueInterface, top:'PrintSemanticSystem'=None) -> List[Tuple[str,ValueInterface]]:
        pass
