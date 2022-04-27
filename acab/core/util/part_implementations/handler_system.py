#!/usr/bin/env python3
"""
Basic Implementations of the handler system protocol
"""
# pylint: disable=abstract-method,invalid-sequence-index,use-a-generator,too-many-lines
# pyright: reportPrivateUsage=warning
from __future__ import annotations
import logging as logmod
from dataclasses import InitVar, dataclass, field, replace
from enum import Enum
from types import MethodType
from typing import (Any, Callable, ClassVar, Generic, Iterable, Iterator,
                    Mapping, Match, MutableMapping, NewType, Protocol,
                    Sequence, Tuple, Type, TypeAlias, TypeVar, cast)

from acab import types as AT
from acab.core.config.config import GET
from acab.core.util.decorators.util import cache
from acab.error.handler import AcabHandlerException
from acab.interfaces import handler_system as HS
from acab.interfaces.data import Structure_i
from acab.interfaces.sieve import AcabSieve
from acab.interfaces.value import Sentence_i, Value_i
from acab.error.protocol import AcabProtocolError as APE
from acab.interfaces.protocols import handler_system as HSubP

logging = logmod.getLogger(__name__)


config = GET()
SPACER = int(config.prepare("Print.Data", "SPACER_SIZE")())
DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()

ModuleComponents   : TypeAlias = AT.ModuleComponents
Overrider          : TypeAlias = AT.HandlerOverride
Sen_A              : TypeAlias = AT.Sentence
Structure          : TypeAlias = "AT.DataStructure[AT.Node]"
Value              : TypeAlias = "AT.Value[AT.ValueCore]"
Handler_A          : TypeAlias = AT.Handler
HandlerSpec_A      : TypeAlias = AT.HandlerSpec
HandlerComponent_A : TypeAlias = AT.HandlerComponent
Handler_System_A   : TypeAlias = AT.HandlerSystem

PASSTHROUGH         = "_"

# Protocols  ##################################################################
@APE.assert_concrete
class HandlerSystem(HS.HandlerSystem_i):
    """
    The Core Handler system, needing only __call__ and extend to be implemented
    """
    @staticmethod
    def Spec(name:str|Sen_A):
        return HandlerSpec(name)

    def __post_init__(self, init_specs, init_handlers, sieve_fns) -> None:
        sieve_fns     = sieve_fns     or self._default_sieve[:] #type:ignore
        init_specs    = init_specs    or []
        init_handlers = init_handlers or []
        try:
            self.sieve = AcabSieve(sieve_fns)
            self._register_spec(*init_specs)
            self._register_default()
            # add handlers with funcs before structs
            self._register_handler(*sorted(init_handlers, key=lambda x: not x.func))
        except AttributeError as err:
            raise AcabHandlerException("Bad Handler in:", rest=init_handlers) from err


    def __contains__(self, signal) -> bool:
        if isinstance(signal, str):
            return signal in self.handler_specs
        elif isinstance(signal, Sentence_i):
            return str(signal) in self.handler_specs
        elif isinstance(signal, (HS.HandlerSpec_i, HS.Handler_i)):
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
        return self.handler_specs[other]

    def __iter__(self):
        for spec in self.handler_specs.values():
            yield spec

    def __repr__(self):
        return f"<{self.__class__.__name__}>"

    def lookup(self, value=None) -> HandlerSpec_A:
        """ run the sieve on the value to get a handler """
        if value is None:
            return self.handler_specs[DEFAULT_HANDLER_SIGNAL]

        is_override    = isinstance(value, HS.HandlerOverride)
        is_passthrough = is_override and value.signal == PASSTHROUGH
        # For using an override to carry data, without an override signal
        if is_passthrough:
            assert(isinstance(value.value, Value_i))
            value = value.value

        for key in self.sieve.fifo(value):
            key_match   = key in self.handler_specs
            if is_override and not is_passthrough and not key_match:
                logging.warning(f"Missing Override Handler: {self.__class__} : {key}")
                continue
            if key is None:
                continue

            assert(isinstance(key, str))
            if key_match and bool(self.handler_specs[key]) and self.handler_specs[key].verify(value):
                # TODO return the handler_spec *if* one of its handler's verifies the instruction
                return self.handler_specs[key]

        # Final resort
        return self.handler_specs[DEFAULT_HANDLER_SIGNAL]

    def override(self, new_signal: bool | str, value, data=None) -> Overrider:
        """ wrap a value to pass data along with it, or explicitly control the signal it produces for handlers """
        # TODO override on an override
        data = data or {}

        if isinstance(value, HS.HandlerOverride):
            updated = value.replace(signal=new_signal or value.signal)
            updated.data.update(data)
            return updated

        match new_signal:
            case str() if new_signal in self:
                pass
            case bool() if not new_signal:
                new_signal = PASSTHROUGH
            case _:
                raise TypeError("Bad Override signal", new_signal)

        return HS.HandlerOverride(new_signal, value, data=data)

    #pylint: disable-next=too-many-branches
    def register(self, *others):
        for other in others:
            match other:
                case HS.HandlerFragment_i():
                    for item in other:
                        self.register(item)
                case HS.HandlerSpec_i():
                    self._register_spec(other)
                case HS.Handler_i():
                    self._register_handler(other)
                case HS.HandlerOverride():
                    raise AcabHandlerException("Attempt to register a HandlerOverride, it should be __call__ed instead")
                case dict():
                    self._register_data(other)
                case _:
                    raise AcabHandlerException("Attempt to register unknown type", rest=[other])

        return self

    def _register_default(self):
        if DEFAULT_HANDLER_SIGNAL in self and bool(self.handler_specs[DEFAULT_HANDLER_SIGNAL]):
            return

        self._register_spec(HandlerSpec(DEFAULT_HANDLER_SIGNAL))



    def _register_spec(self, *specs: HandlerSpec_A):
        for spec in specs:
            as_pseudo = str(spec)
            if as_pseudo in self and spec != self.handler_specs[as_pseudo]:
                raise AcabHandlerException(f"Signal Conflict: {spec.signal}") #type:ignore
            if spec not in self:
                self.handler_specs[as_pseudo] = spec.copy()

        # TODO: Then try to register any loose handlers

    def _register_data(self, data: dict[str, Any], *, signal:None|str=None):
        """
        Register additional data that abstractions may access
        """
        if signal is not None:
            raise NotImplementedError()
            # self.handler_specs[signal].require_data(data)

        self._data.update(data)


    def _register_handler(self, *handlers: Handler_A):
        """
        insert a handler into the system, bound to the signal that it listens for
        """
        for handler in handlers:
            if not isinstance(handler, HS.Handler_i):
                raise AcabHandlerException(f"Handler Not Compliant: {handler}", rest=handler)

            as_pseudo = str(handler)
            if as_pseudo not in self:
                self.loose_handlers.append(handler)
            else:
                self.handler_specs[as_pseudo].register(handler)

    def verify_system(self):
        pass

    def extend(self, modules:list[ModuleComponents]) -> None:
        raise NotImplementedError()
@APE.assert_concrete
class HandlerSpec(HS.HandlerSpec_i):
    def __str__(self):
        return str(self.signal)

    def __repr__(self):
        return f"<HandlerSpec {self.signal}, flags={self.flags}, func_api={self.func_api}, handlers={len(self.handlers)}>"

    def __bool__(self):
        return bool(self.handlers)

    def __eq__(self, other):
        # TODO handle structs
        # TODO api must be equal
        # TODO data and struct api's must be equal
        if isinstance(other, str):
            return str(self) == other
        if isinstance(other, HandlerSpec):
            return self.signal == other.signal

        return False

    def __len__(self):
        return len(self.handlers)

    def __getitem__(self, i):
        return self.handlers[i].func

    def __call__(self, *args, **kwargs) -> None | Any:
        # TODO more advanced logic
        args_l : list[Any] = list(args)
        if len(args_l) < 2:
            args_l.append(self.struct)

        result = None
        for handler in self.handlers:
            result = handler(*args_l, **kwargs)

        return result

    def copy(self, **kwargs) -> HandlerSpec_A:
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
        # TODO refactor spec_from -> from
        assert(self.func_api is None)
        # assert(isinstance(target, (Type, Callable)))
        self.func_api = target
        return self

    def require_data(self, data: list[str]):
        self.data_api += data
        return self

    def require_struct(self, struct: Type[Structure]):
        assert(self.struct_api is None)
        self.struct_api = struct
        return self

    # Create Handler ##########################################################
    def on(self, *, target=None, **kwargs) -> Handler_A:
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
    #pylint: disable-next=too-many-branches
    def register(self, handler: Handler_A) -> None:
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

        if self.handler_limit.stop < len(self.handlers):
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
        if self.func_api is None:
            return

        if isinstance(self.func_api, type):
            if any([x not in dir(func) for x in dir(self.func_api)]):
                raise AcabHandlerException(f"Handler Functional API Mismatch: {self.func_api} against {func}")

        elif isinstance(self.func_api, Callable) and not (func.__code__.co_argcount == self.func_api.__code__.co_argcount): # type: ignore[arg-type]
            raise AcabHandlerException(f"Handler Functional Args API Mismatch: {self.func_api} against {func}")


    def check_struct_api(self, struct):
        if self.struct_api is None:
            return

        is_sub = isinstance(struct, type) and issubclass(struct, self.struct_api)
        is_ins = isinstance(struct, self.struct_api)
        is_eq  = self.struct_api == struct

        if not any([is_sub or is_ins or is_eq]):
            raise AcabHandlerException("Struct api mismatch: {struct} against {self.struct_api}")



@APE.assert_concrete(exceptions=['__call__'])
class HandlerComponent(HS.HandlerComponent_i):

    def verify(self, instruction) -> bool:
        return False

    def as_handler(self, *, signal:None|str|Sen_A=None, struct:None|Structure=None, flags:None|list[Enum]=None) -> Handler_A:
        if signal is None:
            signal = self.signal
        assert(signal is not None)
        return Handler(signal,
                       func=self,
                       struct=struct,
                       flags=set(flags or []))


@APE.assert_concrete
class Handler(HS.Handler_i):

    def __post_init__(self) -> None:
        if isinstance(self.func, type):
            self.func = self.func()

        if self.struct is not None or self.struct_i is None:
            pass
        elif hasattr(self.struct_i, "build_default"):
            self.struct = self.struct_i.build_default() #type:ignore
        else:
            self.struct = self.struct_i() #type:ignore


    def __call__(self, *args, **kwargs):
        if self.func is None:
            raise AcabHandlerException("Attempt to Call Struct Handler", rest=[self])
        return self.func(*args, **kwargs)

    def __iter__(self):
        """ unpack the handler"""
        return (self.func, self.struct).__iter__()

    def __repr__(self):
        sig_s       = str(self.signal)
        func_name   = ""
        struct_name = ""
        if self.func is not None:
            func_name = str(self.func.__class__.__name__) #type:ignore
        if self.struct is not None:
            struct_name = str(self.struct.__class__.__name__)

        return f"<Handler({sig_s}: {func_name}: {struct_name})>"

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

        if not result and self.func is not None and hasattr(self.func, "verify"):
            result = result and self.func.verify(instruction) #type:ignore

        return result

    @cache
    def __str__(self):
        return str(self.signal)

@APE.assert_concrete
class HandlerFragment(HS.HandlerFragment_i):

    def __len__(self):
        return len(self.handlers) + len(self.specs)

    def __repr__(self):
        return f"<Handler Fragment for {self.target_i}: {len(self.specs)} Specs, {len(self.handlers)} Handlers>"

    def __iter__(self):
        for x in self.specs:
            yield x

        for y in self.handlers:
            yield y

    def __contains__(self, other):
        if isinstance(other, HS.HandlerSpec_i):
            return other in self.specs
        elif isinstance(other, HS.Handler_i):
            return other in self.handlers
        else:
            return False
