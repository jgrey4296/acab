#!/usr/bin/env python3
"""
Subprotocols used for Values, Instructions and Sentences

"""
# pylint: disable=abstract-method,too-many-ancestors,multiple-statements,too-few-public-methods,invalid-sequence-index,unnecessary-pass
from __future__ import annotations

import abc
import collections.abc as cABC
from dataclasses import dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Collection,
                    Generic, Iterable, Iterator, Literal, Mapping, Match,
                    MutableMapping, Protocol, Sequence, Tuple, Type, TypeAlias,
                    TypeGuard, TypeVar, cast, runtime_checkable, NewType)
from uuid import UUID, uuid1

if TYPE_CHECKING:
    # tc only imports
    pass

from acab import types as AT
from acab.error.config import AcabConfigException

T     = TypeVar('T')

Value_A       : TypeAlias = "AT.Value[AT.ValueCore_t]"
Value_t       : TypeAlias = Type[Value_A]
Sen_A         : TypeAlias = AT.Sentence
Sen_t         : TypeAlias = Type[Sen_A]
Instruction_A : TypeAlias = AT.Instruction
Variable      = NewType('Variable', Value_A)
ValueData     : TypeAlias = str
GenFunc       : TypeAlias = AT.fns.GenFunc
SemSys_A      : TypeAlias = AT.SemanticSystem


T_Cov = TypeVar('T_Cov', covariant=True)

@dataclass(frozen=True)
class AcabUUID:
    """
    Designate a class as having a unique UUID which
    is tracked through lineages
    """
    _uuid : UUID = field(init=False, default_factory=uuid1)

    @property
    def UUID(self) -> UUID:
        return self._uuid

    def __hash__(self) -> int:
        return hash(self.UUID)

class AcabFinishable_p(Protocol):

    @abc.abstractmethod
    def finish(self) -> Any:
        """
        Idempotent transform for static/mutable structures (like ContextInstances),
        to allow lifting to static/immutable instance
        """
        pass


# Values need to implement all of:
class ValueBasics_p(Protocol):
    """
    A Utility class for default implementations of Value_i methods
    """
    @abc.abstractmethod
    def __str__(self) -> str: pass
    @abc.abstractmethod
    def __repr__(self) -> str: pass
    @abc.abstractmethod
    def __hash__(self) -> int: pass
    @abc.abstractmethod
    def __eq__(self, other:object) -> bool: pass
    @abc.abstractmethod
    def __lt__(self, other:str|Value_A) -> bool: pass
    @abc.abstractmethod
    def copy(self, **kwargs:Any) -> Value_A: pass

class ValueMetaData_p(Protocol):
    """
    Utility Class providing methods for handling value meta data
    """
    @abc.abstractmethod
    def key(self) -> str: pass
    @property
    @abc.abstractmethod
    def type(self) -> Sen_A: pass
    @abc.abstractmethod
    def apply_params(self, *params:Variable, data:None|dict[str,Any]=None) -> Value_A: pass
    @abc.abstractmethod
    def apply_tags(self, *tags:Value_A|Sen_A, data:None|dict[str,Any]=None) -> Value_A: pass
    @abc.abstractmethod
    def has_tag(self, *tags:Value_A) -> bool: pass

class VariableTests_p(Protocol):
    """
    Utility Class providing methods
    for checking if a value is a value
    """
    @property
    @abc.abstractmethod
    def is_var(self) -> bool: pass
    @property
    @abc.abstractmethod
    def is_at_var(self) -> bool: pass
    @property
    @abc.abstractmethod
    def has_var(self) -> bool: pass



# Instructions add:
class AcabReducible_p(Protocol):
    """ Designates a class as being able to reduce down
    to a list of sentences """

    @abc.abstractmethod
    def to_sentences(self) -> list[AT.Sentence]:
        """ Convert to sentences for printing """
        pass


class AcabSentenceable_p(Protocol):
    @abc.abstractmethod
    def attach_statement(self, value:Instruction_A) -> Sen_A: pass
    @abc.abstractmethod
    def detach_statement(self) -> Tuple[Value_A, list[Instruction_A]]: pass

    @staticmethod
    @abc.abstractmethod
    def from_sentences(sens:list[AT.Sentence]) -> list[Any]:
        pass

    def to_word(self) -> Value_A:
        # TODO convert
        return cast(Value_A, self)


# Protocols ###################################################################
@runtime_checkable
class Value_p(ValueBasics_p, ValueMetaData_p, VariableTests_p, Protocol):
    pass


@runtime_checkable
class Sentence_p(Value_p, Collection[Value_p], AcabReducible_p, AcabSentenceable_p, Protocol):
    @abc.abstractmethod
    def __getitem__(self, i:int) -> Value_A|Sen_A: pass
    @abc.abstractmethod
    def add(self, *other: Value_A|Sen_A) -> Sen_A: pass
    @abc.abstractmethod
    def clear(self) -> Sen_A: pass
    @abc.abstractmethod
    def prefix(self, prefix:'Value_A|Sen_A|list[str|Value_A]') -> Sen_A: pass
    @abc.abstractmethod
    def remove_prefix(self, prefix:'Value_A|Sen_A|list[str|Value_A]') -> Sen_A: pass
    @abc.abstractmethod
    def flatten(self, *, rec=False) -> Sen_A: pass

@runtime_checkable
class Instruction_p(Value_p, Collection[Any], AcabReducible_p, AcabSentenceable_p, Protocol):
    @abc.abstractmethod
    def do_break(self) -> None: pass
    # self.breakpoint = not self.breakpoint #type:ignore[has-type]

    @property
    @abc.abstractmethod
    def should_break(self) -> bool: pass
        # return bool(self.breakpoint)

    @property
    def vars(self) -> list[Value_A]:
        return []

@runtime_checkable
class Operator_p(Value_p, Generic[T_Cov], Protocol):
    def is_operator(self) -> Literal[True]: return True
    @abc.abstractmethod
    def __call__(self, *params: Value_A, data:None|dict[str,Any]=None) -> T_Cov: pass


@runtime_checkable
class ActionOperator_p(Value_p, Protocol):
    # Needs to be concrete not a NewType,
    # as its used in isinstance checks
    def is_action(self) -> Literal[True] : return True
    @abc.abstractmethod
    def __call__(self, *params: Value_A, data:None|dict[str,Any]=None, semsys:None|SemSys_A=None) -> None: pass
