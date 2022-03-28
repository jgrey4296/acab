#!/usr/bin/env python
# pylint: disable=line-too-long
from __future__ import annotations

import abc
import logging as logmod
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

from acab.error.base import AcabException

logging = logmod.getLogger(__name__)

if TYPE_CHECKING:
    # tc only imports
    pass

@dataclass
class AcabProtocolError(AcabException):
    """
    Error for reporting when classes do not fully implement their abstract methods or protocols
    """
    target   : type          = field()
    protocol : None|Protocol = field(default=None)

    @staticmethod
    def assert_implements(proto:type, *, exceptions:None|list[str]|set[str]|frozenset[str]=None, error:bool=True) -> Callable[..., type]: #type:ignore
        """
        Raise an Error if the decorated class has abstractmethods found in the protocol
        """
        exceptions = set(exceptions or [])
        assert(hasattr(proto, '__abstractmethods__')), "Protocols must be abstract" #type:ignore
        assert(isinstance(exceptions, set))
        proto_abs : set[str] = set(proto.__abstractmethods__) - exceptions #type:ignore
        def __wrapper(target_cls : type) -> type:
            has_abstract    : bool = hasattr(target_cls, '__abstractmethods__')
            has_all_methods : bool = all([hasattr(target_cls, x) for x in proto_abs])
            if not has_abstract and has_all_methods:
                return target_cls

            if not has_all_methods:
                if error:
                    raise AcabProtocolError(target_cls, proto)
                logging.warning(str(AcabProtocolError(target_cls, proto)))

            if has_abstract and bool(target_cls.__abstractmethods__ & proto_abs): #type:ignore
                if error:
                    raise AcabProtocolError(target_cls, proto)
                logging.warning(str(AcabProtocolError(target_cls, proto)))


            return target_cls

        return __wrapper

    @staticmethod
    def assert_concrete(target_cls:None|type=None, *, exceptions:None|list[str]|set[str]|frozenset[str]=None, error:bool=True) -> Callable[..., type]:
        """
        Raise an Error (or warning) if the decorated class has abstractmethods
        (without waiting till instantiation)
        """
        exceptions = set(exceptions or [])
        assert(isinstance(exceptions, set))
        def __wrapper(target_cls : type) -> type:
            has_abstract    : bool = hasattr(target_cls, '__abstractmethods__')
            if not has_abstract:
                return target_cls

            if bool(target_cls.__abstractmethods__ - exceptions): #type:ignore
                if error:
                    raise AcabProtocolError(target_cls)
                logging.warning(str(AcabProtocolError(target_cls)))

            return target_cls

        if target_cls is None:
            return __wrapper

        return __wrapper(target_cls)


    def __str__(self) -> str:
        assert(hasattr(self.target, "__abstractmethods__"))
        target_s  : str = f"{self.target.__module__}.{self.target.__qualname__}" #type:ignore

        if self.protocol is not None:
            proto_s   : str = f"{self.protocol.__module__}.{self.protocol.__qualname__}" #type:ignore
            missing = ", ".join(self.target.__abstractmethods__ & self.protocol.__abstractmethods__) #type:ignore
            msg = f"Type is Abstract for Protocol: {target_s} : {proto_s}\n\tMissing: {missing}"
        else:
            missing = ", ".join(self.target.__abstractmethods__) #type:ignore
            msg = f"Class has AbstractMethods: {target_s}\n\tMissing: {missing}"

        return msg
