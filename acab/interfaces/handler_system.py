#!/opts/anaconda3/envs/ENV/python
import abc
import logging as root_logger
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from types import MethodType

from acab import types as AT
from acab.interfaces.data import Structure_i
from acab.error.handler_exception import AcabHandlerException
from acab.interfaces.sieve import AcabSieve
from acab.core.config.config import GET

logging = root_logger.getLogger(__name__)


config = GET()
SPACER = int(config.prepare("Print.Data", "SPACER_SIZE")())

pseudo           = AT.pseudo
Handler          = AT.Handler
ModuleComponents = AT.ModuleComponents
Overrider        = AT.HandlerOverride
Sentence         = AT.Sentence
Structure        = AT.DataStructure
Value            = AT.Value

PASSTHROUGH      = "_"

# TODO refactor handler -> responder?
# TODO active and passive handlers?,
# with ability to have multiples for each signal?
@dataclass
class HandlerSystem_i(metaclass=abc.ABCMeta):

    init_handlers  : InitVar[List[Handler]]   = field(default=None)
    # TODO make default  Tuple[str, str], and lookup?
    default        : Handler                  = field(default=None)
    sieve_fns      : InitVar[List[Callable]]  = field(default=None)

    sieve          : AcabSieve                = field(init=False, default=None)
    handlers       : Dict[AT.pseudo, Callable]= field(init=False, default_factory=dict)
    _data          : Dict[str, Any]           = field(init=False, default_factory=dict)

    _default_sieve : ClassVar[List[Callable]] = []

    @dataclass
    class HandlerOverride:
        """ Simple Wrapped for forced semantic use """
        signal   : str              = field()
        value    : Value            = field()
        data     : Dict[Any, Any]   = field(default_factory=dict)


    def __post_init__(self, init_handlers, sieve_fns):
        # TODO handle overriding
        # TODO init any semantics or structs passed in as Class's
        # TODO check depsem -> struct compabilities
        # by running dependent.compatible(struct)
        # use default sieve if sieve is empty
        if not bool(sieve_fns):
            self.sieve = AcabSieve(self._default_sieve)
        else:
            self.sieve = AcabSieve(sieve_fns)

        if init_handlers is None:
            init_handlers = []

        if any([not isinstance(x, (Handler, HandlerComponent_i)) for x in init_handlers]):
            raise AcabHandlerException(f"Bad Handler in:", init_handlers)

        # add handlers with funcs before structs
        for handler in sorted(init_handlers, key=lambda x: not x.func):
            self._register_handler(handler)

    def lookup(self, value:Optional[Value]=None) -> Handler:
        """ run the sieve on the value to get a handler """
        if value is None:
            return self.default

        is_override = isinstance(value, HandlerSystem_i.HandlerOverride)
        is_passthrough = is_override and value.signal == PASSTHROUGH
        # For using an override to carry data, without an override signal
        if is_passthrough:
            value = value.value

        for key in self.sieve.fifo(value):
            key_match   = key in self.handlers

            if is_override and not is_passthrough and not key_match:
                logging.warning(f"Missing Override Handler: {self.__class__} : {key}")
            elif key_match:
                return self.handlers[key]

        # Final resort
        return self.default

    def override(self, new_signal: Union[bool, str], value, data=None) -> Overrider:
        """ wrap a value to pass data along with it, or explicitly control the signal it produces for handlers """
        # TODO override on an override
        if bool(new_signal) and new_signal not in self.handlers:
            raise AcabHandlerException(f"Undefined override handler: {new_signal}")

        if not bool(new_signal):
            new_signal = PASSTHROUGH


        if bool(data):
            return HandlerSystem_i.HandlerOverride(new_signal, value, data=data)

        return HandlerSystem_i.HandlerOverride(new_signal, value)

    def register_data(self, data: Dict[str, Any]):
        """
        Register additional data that abstractions may access
        """
        self._data.update(data)


    def _register_handler(self, handler):
        """
        insert a handler into the system, bound to the signal that it listens for
        """
        if not isinstance(handler, Handler):
            raise AcabHandlerException(f"Handler Not Compliant: {handler}", handler)

        if handler.func is not None:
            self.handlers[handler.signal] = handler


        provides_struct = handler.struct is not None
        has_pair = handler.signal in self.handlers
        pair_needs_struct = has_pair and self.handlers[handler.signal].struct is None

        if provides_struct and pair_needs_struct:
            self.handlers[handler.signal].add_struct(handler.struct)

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


    @staticmethod
    def decorate(signal_name:str, struct=None):
        """ Utility decorator to turn any function into a handler """
        def wrapper(func):
            return HandlerFunction(signal_name,
                                   func=func,
                                   struct=struct)


        return wrapper


    @staticmethod
    def from_method(method, signal=None, struct=None):
        """ A utility function to wrap a single method of a class as a handler.
        Uses passed in signal, otherwise looks for method.__self__.signal
        """
        assert(isinstance(method, MethodType))
        return HandlerFunction(signal or method.__self__.signal,
                               func=method,
                               struct=struct)


    def __post_init__(self):
        if isinstance(self.func, type):
            self.func = self.func()

        if isinstance(self.struct, type) and hasattr(self.struct, "build_default"):
            self.struct = self.struct.build_default()
        elif isinstance(self.struct, type):
            self.struct = self.struct()


    def __call__(self, *args, **kwargs):
        if self.func is None:
            raise AcabHandlerException(f"Attempt to Call Struct Handler", self)
        return self.func(*args, **kwargs)

    def __iter__(self):
        """ unpack the handler"""
        return (self.func, self.struct).__iter__()



    def __str__(self):
        sig_s       = str(self.signal)
        func_name   = ""
        struct_name = ""
        if self.func is not None:
            func_name = str(self.func.__class__.__name__)
        if self.struct is not None:
            struct_name = str(self.struct.__class__.__name__)

        spacer = " " * max(0, (SPACER - len(sig_s)))
        second_spacer = " " * max(0, (SPACER * 2) - (len(spacer) + len(sig_s) + len(func_name)))

        return f"{sig_s}{spacer}{func_name}{second_spacer}{struct_name}"

    def add_struct(self, struct:Structure):
        if isinstance(struct, type) and isinstance(struct, AcabStructure):
            struct = struct.build_default()

        self.struct = struct

@dataclass
class HandlerComponent_i:
    """ Utility Class Component for easy creation of a handler """

    signal : Optional[str] = field(default=None)

    def as_handler(self, signal=None, struct=None):
        assert(signal or self.signal)
        return Handler(signal or self.signal, func=self, struct=struct)
