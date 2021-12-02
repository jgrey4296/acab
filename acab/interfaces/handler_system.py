#!/opts/anaconda3/envs/ENV/python
import abc
import collections.abc as cABC
import logging as root_logger
from dataclasses import InitVar, dataclass, field, replace
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast, Type)
from types import MethodType
from enum import Enum

from acab import types as AT
from acab.interfaces.data import Structure_i
from acab.interfaces.value import Sentence_i
from acab.error.handler_exception import AcabHandlerException
from acab.interfaces.sieve import AcabSieve
from acab.core.config.config import GET

logging = root_logger.getLogger(__name__)


config = GET()
SPACER = int(config.prepare("Print.Data", "SPACER_SIZE")())

pseudo             = AT.pseudo
Handler            = AT.Handler
ModuleComponents   = AT.ModuleComponents
Overrider          = AT.HandlerOverride
Sentence           = AT.Sentence
Structure          = AT.DataStructure
Value              = AT.Value
HandlerSpec        = "HandlerSpec"
HandlerComponent_i = 'HandlerComponent_i'
PASSTHROUGH        = "_"

# TODO active and passive handlers?,
# with ability to have multiples for each signal?
@dataclass
class HandlerSystem_i(metaclass=abc.ABCMeta, cABC.MutableMapping, cABC.Callable):

    init_specs     : InitVar[List[HandlerSpec]] = field(default=None)
    init_handlers  : InitVar[List[Handler]]     = field(default=None)
    # TODO make default  Tuple[str, str], and lookup?
    sieve_fns      : InitVar[List[Callable]]    = field(default=None)

    sieve          : AcabSieve                  = field(init=False, default=None)
    handler_specs  : Dict[pseudo, Callable]     = field(init=False, default_factory=dict)
    loose_handlers : List[Handler]              = field(init=False, default_factory=list)
    _data          : Dict[str, Any]             = field(init=False, default_factory=dict)

    _default_sieve : ClassVar[List[Callable]]   = []

    @dataclass
    class HandlerOverride:
        """ Simple Wrapped for forced semantic use """
        signal   : str              = field()
        value    : Value            = field()
        data     : Dict[Any, Any]   = field(default_factory=dict)


    @staticmethod
    def Spec(name):
        return HandlerSpec(name)

    def __contains__(self, signal: Union[str, Sentence_i, Handler, HandlerSpec]) -> bool:
        if isinstance(signal, str):
            return signal in self.handler_specs
        elif isinstance(signal, Sentence_i):
            return str(signal) in self.handler_specs

        return signal.signal in self.handler_specs

    def __post_init__(self, init_specs, init_handlers, sieve_fns):
        # TODO handle overriding
        # TODO init any semantics or structs passed in as Class's
        # TODO check struct sem -> struct compabilities
        # by running dependent.compatible(struct)
        # use default sieve if sieve is empty

        if not bool(sieve_fns):
            self.sieve = AcabSieve(self._default_sieve)
        else:
            self.sieve = AcabSieve(sieve_fns)

        if init_specs is None:
            init_specs = []
        if init_handlers is None:
            init_handlers = []

        try:
            self.register_spec(*init_specs)
            self.register_default()
            # add handlers with funcs before structs
            self.register_handler(*sorted(init_handlers, key=lambda x: not x.func))
        except AttributeError as err:
            raise AcabHandlerException(f"Bad Handler in:", init_handlers) from err


    def lookup(self, value:Optional[Value]=None) -> HandlerSpec:
        """ run the sieve on the value to get a handler """
        if value is None:
            return self.handler_specs['_:_default']

        is_override = isinstance(value, HandlerSystem_i.HandlerOverride)
        is_passthrough = is_override and value.signal == PASSTHROUGH
        # For using an override to carry data, without an override signal
        if is_passthrough:
            value = value.value

        for key in self.sieve.fifo(value):
            key_match   = key in self.handler_specs

            if is_override and not is_passthrough and not key_match:
                logging.warning(f"Missing Override Handler: {self.__class__} : {key}")
            elif key_match and bool(self.handler_specs[key]):
                return self.handler_specs[key]

        # Final resort
        return self.handler_specs['_:_default']

    def override(self, new_signal: Union[bool, str], value, data=None) -> Overrider:
        """ wrap a value to pass data along with it, or explicitly control the signal it produces for handlers """
        # TODO override on an override
        if bool(new_signal) and new_signal not in self:
            raise AcabHandlerException(f"Undefined override handler: {new_signal}")

        if not bool(new_signal):
            new_signal = PASSTHROUGH


        if bool(data):
            return HandlerSystem_i.HandlerOverride(new_signal, value, data=data)

        return HandlerSystem_i.HandlerOverride(new_signal, value)

    def register_default(self):
        if "_:_default" in self and bool(self.handler_specs["_:_default"]):
            return

        self.register_spec(HandlerSpec("_:_default"))



    def register_spec(self, *specs: HandlerSpec):
        for spec in specs:
            if spec in self and spec != self.handler_specs[spec.signal]:
                raise AcabHandlerException(f"Signal Conflict: {spec.signal}")
            elif spec.signal not in self:
                self.handler_specs[spec.signal] = spec.copy()

        # TODO: Then try to register any loose handlers

    def register_data(self, signal, data: Dict[str, Any]):
        """
        Register additional data that abstractions may access
        """
        assert(signal in self)
        self.handler_specs[signal].register_data(data)


    def register_handler(self, *handlers: Handler):
        """
        insert a handler into the system, bound to the signal that it listens for
        """
        for handler in handlers:
            if not isinstance(handler, Handler):
                raise AcabHandlerException(f"Handler Not Compliant: {handler}", handler)

            if handler.signal not in self:
                logging.warning(f"Unexpected handler in register area. Please check {handler.signal}")
                self.loose_handlers.append(handler)
            else:
                self.handler_specs[handler.signal]._register(handler)

    def verify(self):
        pass

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
class HandlerSpec(cABC.MutableSequence, cABC.Callable):


    signal              : Union[str, Sentence_i]
    flags               : List[Enum]                    = field(default_factory=list)
    func_api            : Union[Type[Any], Callable]    = field(default=None)
    struct_api          : Union[Type[Any], Structure_i] = field(default=None)
    data_api            : List[str]                     = field(default_factory=list)

    registered_data     : Dict[str, Any]                = field(init=False, default_factory=dict)
    registered_handlers : List[Handler]                 = field(init=False, default_factory=list)
    registered_struct   : Optional[Callable]            = field(init=False, default=None)

    def __repr__(self):
        return f"HandlerSpec({self.signal}, flags={self.flags}, func_api={self.func_api}, handlers={len(self.registered_handlers)})"

    def __bool__(self):
        return bool(self.registered_handlers)
    def __eq__(self, other: HandlerSpec):
        # TODO handle structs
        # TODO api must be equal
        # TODO data and struct api's must be equal
        return self.signal == other.signal

    def __getitem__(self, i):
        return self.registered_handlers[i].func

    def spec_from(self, target):
        """
        Decorator to use as the spec's interface.
        ie:
        @a_spec(signal).from
        class An_ABC:...

        or:
        @a_spec(signal).from
        def a_func...

        """
        assert(self.func_api is None)
        assert(isinstance(target, (Type, Callable)))
        self.func_api = target
        return self

    def require_data(self, data: List[str]):
        self.data_api += data
        return self

    def require_struct(self, struct: Structure):
        assert(self.struct_api is None)
        self.struct_api = struct
        return self

    def on(self, target, *args, **kwargs) -> Handler:
        """
        basicly create a Handler.
        The inverse decorator of `from`.
        ie:
        @a_spec.on
        class Implements(An_ABC):...

        or
        @a_spec.on
        def an_implementing_func....
        """
        # TODO check target against spec
        return Handler(self.signal, target, **kwargs)

    def _register(self, handler: Handler):
        """ Add a handler into the current, according to the spec instructions
        and handler's flags """
        # And check types
        provides_struct = handler.struct is not None
        needs_struct    = self.registered_struct is None

        if provides_struct and needs_struct:
            self.add_struct(handler.struct)
        elif provides_struct and not needs_struct:
            raise AcabHandlerException(f"{self.signal} struct conflict")

        if handler.func is not None:
            self.registered_handlers.append(handler)

    def add_struct(self, struct:Structure):
        if isinstance(struct, type) and isinstance(struct, AcabStructure):
            struct = struct.build_default()

        self.registered_struct = struct

    def verify(self):
        """
        Check all registered handlers against the api
        Check all registered data against it's api
        Check the registered struct against it's api
        """
        # typing.get_type_hints
        pass
    def check_api(self, func=None, data=None, struct=None):
        if func and self.func_api:
            is_sub = issubclass(self.func_api, func)
            is_ins = isinstance(self.func_api, func)
            is_eq  = self.func_api == func
            return is_sub or is_ins or is_eq
        elif data and self.data_api:
            return all([x in self.data_api for x in data])
        elif struct and self.struct_api:
            is_sub = issubclass(self.struct_api, struct)
            is_ins = isinstance(self.struct_api, struct)
            is_eq  = self.struct_api == struct
            return is_sub or is_ins or is_eq
        else:
            return False

    def run(self, *args, **kwargs):
        return self.registered_handlers

    def copy(self, **kwargs) -> HandlerSpec:
        return replace(self,
                       func_api=self.func_api,
                       struct_api=self.struct_api,
                       data_api=self.data_api)

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
            return Handler(signal_name,
                           func=func,
                           struct=struct)


        return wrapper


    @staticmethod
    def from_method(method, signal=None, struct=None):
        """ A utility function to wrap a single method of a class as a handler.
        Uses passed in signal, otherwise looks for method.__self__.signal
        """
        assert(isinstance(method, MethodType))
        return Handler(signal or method.__self__.signal,
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



    def __repr__(self):
        sig_s       = str(self.signal)
        func_name   = ""
        struct_name = ""
        if self.func is not None:
            func_name = str(self.func.__class__.__name__)
        if self.struct is not None:
            struct_name = str(self.struct.__class__.__name__)

        return f"Handler({sig_s:{SPACER}}{func_name:{SPACER}}{struct_name})"


    def as_handler(self, signal=None, struct=None):
        return Handler(signal or self.signal, func=self.func, struct=struct or self.struct)
@dataclass
class HandlerComponent_i:
    """ Utility Class Component for easy creation of a handler """

    signal : Optional[str] = field(default=None)

    def as_handler(self, signal=None, struct=None):
        assert(signal or self.signal), breakpoint()
        return Handler(signal or self.signal, func=self, struct=struct)
