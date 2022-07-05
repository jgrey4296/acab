"""

"""
from __future__ import annotations

import abc
import logging as logmod
from dataclasses import InitVar, dataclass, field
from types import FunctionType
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from uuid import UUID, uuid1

from acab import types as AT
from acab.error.protocol import AcabProtocolError

if TYPE_CHECKING:
    # tc only imports
    pass


logging = logmod.getLogger(__name__)
T = TypeVar('T')

class AbsProtocolMeta(type):
    """
    A Metaclass which creates a protocol,
    and wraps every method defined in abstractmethod
    """

    def __new__(cls, name:str, bases:tuple[type, ...], definitions:dict[str,Any]) -> Any:
        # Check the code of the function is just the "pass" instruction.. I think
        pass_code = b'd\x00S\x00'
        for k,v in definitions.items():
            assert(isinstance(v, FunctionType) and v.__code__.co_code == pass_code)
            new_abstract = abc.abstractmethod(v)
            definitions[k] = new_abstract

        if bool(bases):
            bases = (*bases, Protocol) #type:ignore
        else:
            bases = (Protocol,) #type:ignore
        new_protocol = type(name, bases, definitions)

        return new_protocol



def abs_protocol(cls : T) -> T:
    """ A class decorator for automatically creating abstractmethods """
    pass_code           = b'd\x00S\x00'
    abstract : set[str] = set()
    for k,v in cls.__dict__.items():
        if (isinstance(v, FunctionType) and v.__code__.co_code == pass_code):
            new_abstract = abc.abstractmethod(v)
            setattr(cls, k, new_abstract)
            abstract.add(k)

    setattr(cls, '__abstractmethods__', frozenset(abstract))
    return cls

