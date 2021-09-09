#!/opts/anaconda3/envs/ENV/python
import abc
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from types import MethodType

from acab.abstract.interfaces.data import Structure_i
from acab.error.acab_base_exception import AcabBaseException
from acab.abstract.interfaces.util import LogDecorator

logging = root_logger.getLogger(__name__)

Handler          = 'HandlerComponent_i'
ModuleComponents = 'ModuleComponents'
Overrider        = 'HandlerOverride'
Sentence         = 'Sentence'
Structure        = Structure_i
Value_i          = 'Value_i'

# TODO refactor handler -> responder?
# TODO active and passive handlers?,
# with ability to have multiples for each signal?
@dataclass
class HandlerSystem_i(metaclass=abc.ABCMeta):

    in_handlers    : InitVar[List[Handler]]   = field(default=None)
    # TODO make default  Tuple[str, str], and lookup?
    default        : Handler                  = field(default=None)
    sieve          : List[Callable]           = field(default_factory=list)

    handlers       : Dict[str, Callable]      = field(init=False, default_factory=dict)
    _data          : Dict[str, Any]           = field(init=False, default_factory=dict)

    _default_sieve : ClassVar[List[Callable]] = []

    @dataclass
    class HandlerOverride:
        """ Simple Wrapped for forced semantic use """
        override : str              = field()
        data     : Value_i   = field()


    def __post_init__(self, in_handlers):
        # TODO handle overriding
        # TODO init any semantics or structs passed in as Class's
        # TODO check depsem -> struct compabilities
        # by running dependent.compatible(struct)
        # use default sieve if sieve is empty
        if not bool(self.sieve):
            self.sieve += self._default_sieve

        if in_handlers is None:
            in_handlers = []

        if any([not isinstance(x, Handler) for x in in_handlers]):
            raise AcabBaseException(f"Bad Handler in:", in_handlers)

        # add handlers with funcs before structs
        for handler in sorted(in_handlers, key=lambda x: not x.func):
            self._register_handler(handler)

    def _register_handler(self, handler):
        if not isinstance(handler, Handler):
            raise AcabBaseException(f"Handler Not Compliant: {handler}", handler)

        if handler.func is not None:
            self.handlers[handler.signal] = handler


        provides_struct = handler.struct is not None
        has_pair = handler.signal in self.handlers
        pair_needs_struct = has_pair and self.handlers[handler.signal].struct is None

        if provides_struct and pair_needs_struct:
            self.handlers[handler.signal].add_struct(handler.struct)

    #@LogDecorator("Using Semantics: ")
    def lookup(self, value: Value_i) -> Handler:
        # sieve from most to least specific
        if value is None:
            return self.default

        for sieve_fn in self.sieve:
            key = sieve_fn(value)
            if not bool(key):
                continue

            if key in self.handlers:
                return self.handlers[key]

        # Final resort
        return self.default

    def override(self, new_target: str, value) -> Overrider:
        if new_target not in self.handlers:
            raise AcabBaseException(f"Undefined override handler: {new_target}")

        return HandlerSystem_i.HandlerOverride(new_target, value)

    def register_data(self, data: Dict[str, Any]):
        """
        Register additional data that abstractions may access
        """
        self._data.update(data)

    @abc.abstractmethod
    def extend(self, modules:List[ModuleComponents]):
        """ Abstract because different handlers use
        different module components """
        pass
    @abc.abstractmethod
    def __call__(self, *args, **kwargs):
        pass

#--------------------
@dataclass
class Handler:
    """ A Handler implementation for registering
    individual functions or methods """

    signal : Union[Sentence, str] = field()
    func   : Optional[Callable]   = field(default=None)
    struct : Optional[Callable]   = field(default=None)

    def __post_init__(self):
        if isinstance(self.func, type):
            self.func = self.func()

        if isinstance(self.struct, type):
            self.struct = self.struct.build_default()


    def __call__(self, *args, **kwargs):
        if self.func is None:
            raise AcabBaseException(f"Attempt to Call Struct Handler", self)
        return self.func(*args, **kwargs)

    def add_struct(self, struct:Structure):
        if isinstance(struct, type) and isinstance(struct, AcabStructure):
            struct = struct.build_default()

        self.struct = struct

    @staticmethod
    def decorate(signal_name:str, struct=None):
        def wrapper(func):
            return HandlerFunction(signal_name,
                                   func=func,
                                   struct=struct)

        return wrapper

    @staticmethod
    def from_method(func, signal=None, struct=None):
        assert(isinstance(func, MethodType))
        return HandlerFunction(signal or func.__self__.signal,
                               func=func,
                               struct=struct)



    def to_pair(self):
        return (self.func, self.struct)
