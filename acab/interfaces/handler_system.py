#!/opts/anaconda3/envs/ENV/python
import abc
import collections.abc as cABC
import logging as root_logger
from dataclasses import InitVar, dataclass, field, replace
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, Sequence,
                    Tuple, TypeVar, cast, Type, TypeAlias, NewType)
from types import MethodType
from enum import Enum

from acab import types as AT
from acab.interfaces.data import Structure_i
from acab.interfaces.value import Sentence_i
from acab.error.handler import AcabHandlerException
from acab.interfaces.sieve import AcabSieve
from acab.core.config.config import GET
from acab.core.decorators.util import cache

logging = root_logger.getLogger(__name__)


config = GET()
SPACER = int(config.prepare("Print.Data", "SPACER_SIZE")())
DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()

pseudo             : TypeAlias = AT.pseudo
Handler            : TypeAlias = AT.Handler
ModuleComponents   : TypeAlias = AT.ModuleComponents
Overrider          : TypeAlias = AT.HandlerOverride
Sentence           : TypeAlias = AT.Sentence
Structure          : TypeAlias = AT.DataStructure
Value              : TypeAlias = AT.Value
HandlerSpec        : TypeAlias = NewType("HandlerSpec", Any)
HandlerComponent_i : TypeAlias = NewType('HandlerComponent_i', Any)
ContainParams      : TypeAlias = str | Sentence | Handler | HandlerSpec

PASSTHROUGH        : TypeAlias = "_"

@dataclass
class HandlerSystem_i(cABC.MutableMapping, cABC.Callable):

    init_specs     : InitVar[list[HandlerSpec]] = field(default=None)
    init_handlers  : InitVar[list[Handler]]     = field(default=None)
    # TODO make default  Tuple[str, str], and lookup?
    sieve_fns      : InitVar[list[Callable]]    = field(default=None)

    sieve          : AcabSieve                  = field(init=False, default=None)
    handler_specs  : dict[pseudo, HandlerSpec]  = field(init=False, default_factory=dict)
    loose_handlers : list[Handler]              = field(init=False, default_factory=list)
    _data          : dict[str, Any]             = field(init=False, default_factory=dict)

    _default_sieve : ClassVar[list[Callable]]   = [str]

    @dataclass
    class HandlerOverride:
        """ Simple Wrapper for forced semantic use
        ie: a continuation
        """
        signal   : str              = field()
        value    : Value            = field()
        data     : dict[Any, Any]   = field(default_factory=dict)


    @staticmethod
    def Spec(name):
        return HandlerSpec(name)

    def __post_init__(self, init_specs, init_handlers, sieve_fns):
        sieve_fns     = sieve_fns     or self._default_sieve[:]
        init_specs    = init_specs    or list()
        init_handlers = init_handlers or list()

        try:
            self.sieve = AcabSieve(sieve_fns)
            self._register_spec(*init_specs)
            self._register_default()
            # add handlers with funcs before structs
            self._register_handler(*sorted(init_handlers, key=lambda x: not x.func))
        except AttributeError as err:
            raise AcabHandlerException(f"Bad Handler in:", init_handlers) from err


    def __contains__(self, signal: ContainParams) -> bool:
        if isinstance(signal, str):
            return signal in self.handler_specs
        elif isinstance(signal, Sentence_i):
            return str(signal) in self.handler_specs
        elif isinstance(signal, (HandlerSpec, Handler)):
            return str(signal) in self.handler_specs
        else:
            raise ValueError(f"Unrecognised signal value: {signal}")

    def __bool__(self):
        return 1 < len(self.handler_specs)

    def __len__(self):
        return len(self.handler_specs)

    def __getitem__(self, other):
        if not isinstance(other, str):
            raise ValueError(f"Bad Signal Attempt to HandlerSystem: {other}")
        return self.handler_specs[str(other)]

    def __delitem__(self, key):
        raise NotImplementedError("HandlerSystems should be monotonoic")

    def __setitem__(self, key, value):
        raise NotImplementedError("HandlerSystems should modify through `register` and `extend`")

    def __iter__(self):
        for spec in self.handler_specs.values():
            yield spec

    def __repr__(self):
        return f"<{self.__class__.__name__}>"

    def lookup(self, value:[Value|None]=None) -> HandlerSpec:
        """ run the sieve on the value to get a handler """
        if value is None:
            return self.handler_specs[DEFAULT_HANDLER_SIGNAL]

        is_override    = isinstance(value, HandlerSystem_i.HandlerOverride)
        is_passthrough = is_override and value.signal == PASSTHROUGH
        # For using an override to carry data, without an override signal
        if is_passthrough:
            value = value.value

        for key in self.sieve.fifo(value):
            key_match   = key in self.handler_specs
            if is_override and not is_passthrough and not key_match:
                logging.warning(f"Missing Override Handler: {self.__class__} : {key}")
            elif key_match and bool(self.handler_specs[key]) and self.handler_specs[key].verify(value):
                # TODO return the handler_spec *if* one of its handler's verifies the instruction
                return self.handler_specs[key]

        # Final resort
        return self.handler_specs[DEFAULT_HANDLER_SIGNAL]

    def override(self, new_signal: bool | str, value, data=None) -> Overrider:
        """ wrap a value to pass data along with it, or explicitly control the signal it produces for handlers """
        # TODO override on an override
        if bool(new_signal) and new_signal not in self:
            raise AcabHandlerException(f"Undefined override handler: {new_signal}")

        if not bool(new_signal):
            new_signal = PASSTHROUGH


        if bool(data):
            return HandlerSystem_i.HandlerOverride(new_signal, value, data=data)

        return HandlerSystem_i.HandlerOverride(new_signal, value)

    def register(self, *others):
        for other in others:
            if isinstance(other, Handler_Fragment):
                for item in other:
                    self.register(item)
            elif isinstance(other, HandlerSpec):
                self._register_spec(other)
            elif isinstance(other, Handler):
                self._register_handler(other)
            elif isinstance(other, HandlerSystem_i.HandlerOverride):
                raise AcabHandlerException("Attempt to register a HandlerOverride, it should be __call__ed instead")
            elif isinstance(other, dict):
                self._register_data(other)

        return self

    def _register_default(self):
        if DEFAULT_HANDLER_SIGNAL in self and bool(self.handler_specs[DEFAULT_HANDLER_SIGNAL]):
            return

        self._register_spec(HandlerSpec(DEFAULT_HANDLER_SIGNAL))



    def _register_spec(self, *specs: HandlerSpec):
        for spec in specs:
            if str(spec) in self and spec != self.handler_specs[str(spec)]:
                raise AcabHandlerException(f"Signal Conflict: {spec.signal}")
            elif spec not in self:
                self.handler_specs[str(spec)] = spec.copy()

        # TODO: Then try to register any loose handlers

    def _register_data(self, data: dict[str, Any], *, signal:str=None):
        """
        Register additional data that abstractions may access
        """
        if signal is not None:
            self.handler_specs[signal]._register_data(data)
        else:
            self._data.update(data)


    def _register_handler(self, *handlers: Handler):
        """
        insert a handler into the system, bound to the signal that it listens for
        """
        for handler in handlers:
            if not isinstance(handler, Handler):
                raise AcabHandlerException(f"Handler Not Compliant: {handler}", handler)

            if str(handler) not in self:
                self.loose_handlers.append(handler)
            else:
                self.handler_specs[str(handler)].register(handler)

    def verify(self):
        pass

    @abc.abstractmethod
    def extend(self, modules:list[ModuleComponents]):
        """ Abstract because different handlers use
        different module components """
        pass
    @abc.abstractmethod
    def __call__(self, *args, **kwargs):
        pass

#--------------------
@dataclass
class HandlerSpec(cABC.MutableSequence, cABC.Callable):

    signal        : str | Sentence_i | Enum
    flags         : list[Enum]              = field(default_factory=list)
    func_api      : Type[Any] | Callable   = field(default=None)
    struct_api    : Type[Any] | Structure_i = field(default=None)
    data_api      : list[str]               = field(default_factory=list)
    handler_limit : slice                   = field(default=None)

    data            : dict[str, Any]        = field(init=False, default_factory=dict)
    handlers        : list[Handler]         = field(init=False, default_factory=list)
    struct          : None | Callable       = field(init=False, default=None)
    h_limit_history : bool                  = field(init=False, default=False)

    flag_e              : Enum = Enum("HandlerFlags", "OVERRIDE MERGE APPEND PREPEND COLLECT REDUCE")

    @cache
    def __str__(self):
        return str(self.signal)

    def __repr__(self):
        return f"<HandlerSpec {self.signal}, flags={self.flags}, func_api={self.func_api}, handlers={len(self.handlers)}>"

    def __bool__(self):
        return bool(self.handlers)

    def __eq__(self, other: HandlerSpec):
        # TODO handle structs
        # TODO api must be equal
        # TODO data and struct api's must be equal
        if isinstance(other, str):
            return str(self) == other

        return self.signal == other.signal

    def __len__(self):
        return len(self.handlers)

    def __getitem__(self, i):
        return self.handlers[i].func

    def __delitem__(self, key):
        raise NotImplementedError("Handler Spec's should not delete items")

    def __setitem__(self, key, val):
        raise NotImplementedError("Handler Spec's should not set items")

    def __call__(self, *args, **kwargs) -> None | Any:
        # TODO more advanced logic
        if len(args) < 2:
            args = list(args) + [self.struct]

        result = None
        for handler in self.handlers:
            result = handler(*args, **kwargs)

        return result

    def insert(self, item):
        raise NotImplementedError("Handler Spec's should `register` handlers through a HandlerSystem")

    def copy(self, **kwargs) -> HandlerSpec:
        return replace(self,
                       func_api=self.func_api,
                       struct_api=self.struct_api,
                       data_api=self.data_api)

    # set APIs ################################################################
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

    def require_data(self, data: list[str]):
        self.data_api += data
        return self

    def require_struct(self, struct: Structure):
        assert(self.struct_api is None)
        self.struct_api = struct
        return self

    # Create Handler ##########################################################
    def on(self, *, target=None, **kwargs) -> Handler:
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

    # Register Handler ########################################################
    def register(self, handler: Handler):
        """ Add a handler into the current, according to the spec instructions
        and handler's flags """
        # And check types
        if handler.struct is not None:
            self.add_struct(handler)

        if self.flag_e.OVERRIDE in handler.flags and handler.func is not None:
            self.handlers = [handler]
        elif handler.func is not None:
            self.check_api(func=handler.func)
            self.handlers.append(handler)

        if self.handler_limit is None:
            return

        # once you hit the minimum number of handlers, you can't go lower
        if not self.h_limit_history and self.handler_limit.start <= len(self.handlers):
            self.h_limit_history = True

        if self.h_limit_history and len(self.handlers) < self.handler_limit.start:
            raise AcabHandlerException("Handlers Lower Limit Failure")
        elif self.handler_limit.stop < len(self.handlers):
            raise AcabHandlerException("Handlers Upper Limit Failure")



    def add_struct(self, handler):
        struct = handler.struct
        if isinstance(struct, type) and isinstance(struct, Structure_i):
            struct = struct.build_default()

        if self.struct is None or self.flag_e.OVERRIDE in handler.flags:
            self.check_api(struct=struct)
            self.struct = struct
        elif self.struct is not None:
            raise AcabHandlerException(f"{self.signal} struct conflict")


    # Check ###################################################################
    def verify(self, instruction) -> bool:
        """
        Check at least 1 handler can accept the instruction
        """
        if not bool(self):
            return False
        return any([x.verify(instruction) for x in self.handlers])

    def check_api(self, *, func=None, data=None, struct=None):
        if func and self.func_api:
            self.check_func_api(func)
        elif data and self.data_api and any([x not in self.data_api for x in data]):
            raise AcabHandlerException("Data api mismatch: {data} against {self.data_api}")
        elif struct is not None and self.struct_api is not None:
            self.check_struct_api(struct)

        return True

    def check_func_api(self, func):
        if isinstance(self.func_api, Type):
            if any([x not in dir(func) for x in dir(self.func_api)]):
                raise AcabHandlerException(f"Handler Functional API Mismatch: {self.func_api} against {func}")

        elif isinstance(self.func_api, Callable) and not (func.__code__.co_argcount == self.func_api.__code__.co_argcount):
            raise AcabHandlerException(f"Handler Functional Args API Mismatch: {self.func_api} against {func}")


    def check_struct_api(self, struct):
        is_sub = isinstance(struct, Type) and issubclass(struct, self.struct_api)
        is_ins = isinstance(struct, self.struct_api)
        is_eq  = self.struct_api == struct

        if not any([is_sub or is_ins or is_eq]):
            raise AcabHandlerException("Struct api mismatch: {struct} against {self.struct_api}")


@dataclass
class HandlerComponent_i:
    """ Utility Class Component for easy creation of a handler """

    signal : None | str = field(default=None)

    def verify(self, instruction) -> bool:
        return False

    def as_handler(self, *, signal=None, struct=None, flags=None):
        assert(signal or self.signal), breakpoint()
        return Handler(signal or self.signal,
                       func=self,
                       struct=struct,
                       flags=flags or list())

@dataclass
class Handler(cABC.Callable, cABC.Iterable):
    """ A Handler implementation for registering
    individual functions or methods """

    signal   : Sentence | str
    func     : None | Callable     = field(default=None)
    struct_i : None | Callable     = field(default=None)
    verify_f : None | Callable     = field(default=None)
    flags    : set[Enum]           = field(default_factory=set)

    struct   : None | Structure    = field(default=None)

    def __post_init__(self):
        if isinstance(self.func, type):
            self.func = self.func()

        if isinstance(self.struct_i, type) and hasattr(self.struct_i, "build_default"):
            self.struct = self.struct_i.build_default()
        elif isinstance(self.struct, type):
            self.struct = self.struct_i()


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

        return f"Handler({sig_s}: {func_name}: {struct_name})"

    def as_handler(self, *, signal=None, struct=None, flags=None):
        return Handler(signal or self.signal,
                       func=self.func,
                       struct=struct or self.struct,
                       flags=flags or self.flags)


    def verify(self, instruction):
        result = False
        if self.verify_f is not None:
            result = self.verify_f(instruction)
        else:
            result = True

        if hasattr(self.func, "verify"):
            result = result and self.func.verify(instruction)

        return result
    @cache
    def __str__(self):
        return str(self.signal)

# Modules #####################################################################
@dataclass
class Handler_Fragment(cABC.Sized, cABC.Iterable):
    """ Structure of Handlers to be added to a system, and any
    data they require
    """
    specs       : list[HandlerSpec]        = field(default_factory=list)
    handlers    : list[Handler]            = field(default_factory=list)
    target_i    : HandlerSystem_i          = field(default=HandlerSystem_i)


    def __len__(self):
        return len(self.handlers) + len(self.specs)

    def __repr__(self):
        return f"(Handler Fragment for {self.target_i}: {len(self.specs)} Specs, {len(self.handlers)} Handlers)"

    def __iter__(self):
        for x in self.specs:
            yield x

        for y in self.handlers:
            yield y
