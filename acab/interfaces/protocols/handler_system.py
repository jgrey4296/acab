#!/opts/anaconda3/envs/ENV/python
# pylint: disable=multiple-statements,abstract-method,invalid-sequence-index,too-few-public-methods
from __future__ import annotations
import abc
import collections.abc as cABC
import logging as logmod
from dataclasses import InitVar, dataclass, field, replace
from enum import Enum, EnumMeta
from types import MethodType
from typing import (Any, Callable, ClassVar, Collection, Container, Final,
                    Generic, Iterable, Iterator, Mapping, Match,
                    MutableMapping, NewType, Protocol, Sequence, Tuple, Type,
                    TypeAlias, TypeVar, cast, final, overload,
                    runtime_checkable)

from acab import AcabConfig

logging = logmod.getLogger(__name__)
config  = AcabConfig()

from typing import TYPE_CHECKING
if TYPE_CHECKING:
    # tc only imports
    from acab import types as AT
    T = TypeVar('T')
    GenFunc               : TypeAlias = AT.fns.GenFunc
    HandlerComponent_A    : TypeAlias = AT.HandlerComponent
    HandlerFragment_A     : TypeAlias = AT.HandlerFragment
    HandlerOverride_A     : TypeAlias = AT.HandlerOverride
    HandlerSpec_A         : TypeAlias = AT.HandlerSpec
    Handler_A             : TypeAlias = AT.Handler
    Handler_System_A      : TypeAlias = AT.HandlerSystem
    Instruction_A         : TypeAlias = AT.Instruction
    ModuleFragment        : TypeAlias = AT.ModuleFragment
    RegistrationTargets_A : TypeAlias = "HandlerFragment_A|HandlerSpec_A|Handler_A|HandlerOverride_A|dict[str,Any]"
    Sen_A                 : TypeAlias = AT.Sentence
    Structure             : TypeAlias = "AT.DataStructure[AT.Node]"
    Structure_t           : TypeAlias = Type[Structure]
    Value_A               : TypeAlias = "AT.Value[AT.ValueCore]"
else:
    HandlerSpec_A = "HandlerSpec_i"


# Protocols  ##################################################################
@runtime_checkable
class HandlerSystem_p(Iterable[HandlerSpec_A], Protocol):
    # pylint: disable=invalid-name

    @staticmethod
    @abc.abstractmethod
    def Spec(name:str|Sen_A) -> HandlerSpec_A: pass

    @abc.abstractmethod
    def __bool__(self) -> bool: pass
    @abc.abstractmethod
    def __contains__(self, signal:str|Sen_A|HandlerSpec_A|Handler_A) -> bool: pass
    @abc.abstractmethod
    def __getitem__(self, other:str|Sen_A) -> HandlerSpec_A: pass
    @abc.abstractmethod
    def __iter__(self) -> Iterator[HandlerSpec_A]: pass
    @abc.abstractmethod
    def __len__(self) -> int: pass
    @abc.abstractmethod
    def __repr__(self) -> str: pass
    @abc.abstractmethod
    def extend(self, modules:list[ModuleFragment]) -> None: pass
    @abc.abstractmethod
    def lookup(self, value:None|Value_A|HandlerOverride_A=None) -> HandlerSpec_A: pass
    @abc.abstractmethod
    def override(self, new_signal: bool | str, value:Any, data:None|dict[str,Any]=None) -> HandlerOverride_A: pass
    @abc.abstractmethod
    def register(self, *others:RegistrationTargets_A) -> Handler_System_A: pass
    @abc.abstractmethod
    def verify_system(self) -> None: pass

    @property
    @abc.abstractmethod
    def signals(self) -> list[str]: pass

@runtime_checkable
class HandlerSpec_p(Protocol):
    @abc.abstractmethod
    def __bool__(self) -> bool: pass
    @abc.abstractmethod
    def __call__(self, *args:Any, **kwargs:Any) -> None | Any: pass
    @abc.abstractmethod
    def __eq__(self, other: object) -> bool: pass
    @abc.abstractmethod
    def __getitem__(self, i:int) -> None | HandlerComponent_A | GenFunc: pass
    @abc.abstractmethod
    def __len__(self) -> int: pass
    @abc.abstractmethod
    def __repr__(self) -> str: pass
    @abc.abstractmethod
    def __str__(self) -> str: pass
    @abc.abstractmethod
    def add_struct(self, handler:Handler_A) -> None: pass
    @abc.abstractmethod
    def check_api(self, *, func:None|GenFunc=None, data:None|dict[str,Any]=None, struct:Any=None) -> bool: pass
    @abc.abstractmethod
    def check_func_api(self, func:Type[Any]|GenFunc) -> None: pass
    @abc.abstractmethod
    def check_struct_api(self, struct:Any) -> None: pass
    @abc.abstractmethod
    def copy(self, **kwargs:Any) -> HandlerSpec_A: pass
    @abc.abstractmethod
    def on(self, *, target:None|GenFunc=None, **kwargs:Any) -> Handler_A: pass
    @abc.abstractmethod
    def register(self, handler: Handler_A) -> None: pass
    @abc.abstractmethod
    def require_data(self, data: list[str]) -> HandlerSpec_A: pass
    @abc.abstractmethod
    def require_struct(self, struct: Structure_t) -> HandlerSpec_A: pass
    @abc.abstractmethod
    def spec_from(self, target:GenFunc) -> HandlerSpec_A: pass
    @abc.abstractmethod
    def verify(self, instruction: Value_A|Instruction_A|HandlerOverride_A) -> bool: pass

@runtime_checkable
class HandlerComponent_p(Protocol):
    @abc.abstractmethod
    def as_handler(self, *, signal:None|str|Sen_A=None, struct:None|Structure=None, flags:None|list[Enum]=None) -> Handler_A: pass
    @abc.abstractmethod
    def verify(self, instruction:Value_A|Instruction_A|HandlerOverride_A) -> bool: pass

    @abc.abstractmethod
    def __call__(self, *args:Any, **kwargs:Any) -> Any: pass

@runtime_checkable
class Handler_p(Iterable[Any], Protocol):
    @abc.abstractmethod
    def __call__(self, *args:Any, **kwargs:dict[str,Any]) -> Any: pass
    @abc.abstractmethod
    def __iter__(self) -> Iterator[Any]: pass
    @abc.abstractmethod
    def __repr__(self) -> str: pass
    @abc.abstractmethod
    def __str__(self) -> str: pass
    @abc.abstractmethod
    def as_handler(self, *, signal:None|str|Sen_A=None, struct:None|Structure=None, flags:None|list[Enum]=None) -> Handler_A: pass
    @abc.abstractmethod
    def verify(self, instruction:Value_A|Instruction_A|HandlerOverride_A) -> bool: pass

