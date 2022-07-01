"""
Exception types raised by type checking
"""
from __future__ import annotations
#https://docs.python.org/3/library/dataclasses.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab import types as AT
from dataclasses import dataclass, field, InitVar
from acab.error.base import AcabException

@dataclass
class AcabTypingException(AcabException):

    left  : AT.Value | AT.Sentence
    right : AT.Value | AT.Sentence
    ctx   : AT.CtxIns      = field(repr=False, kw_only=True)
    data  : dict[str, Any] = field(default_factory=dict, repr=False, kw_only=True)
    msg   : str            = field(default="Abstract Typing Exception", repr=False, kw_only=True)

    def __str__(self):
        return self.msg

@dataclass(repr=False)
class AcabLengthUnifyException(AcabTypingException):
    msg : str = field(default="Unification length mismatch")

@dataclass(repr=False)
class AcabUnifyGroupException(AcabTypingException):
    msg : str = field(default="Group Unification Failed")

    def __str__(self):
        if 'failures' not in self.data:
            return self.msg
        if len(self.data['failures']) > 1:
            return f"{self.msg}: {len(self.data['failures'])} issues."

        return f"{self.msg}: {str(self.data['failures'][0])}"

@dataclass(repr=False)
class AcabUnifyModalityException(AcabTypingException):
    msg : str = field(default="Modality Failure")

    def __str__(self):
        return f"{self.msg}: {self.data['modality']} : {self.left} : {self.right}"

@dataclass(repr=False)
class AcabUnifySieveFailure(AcabTypingException):
    msg : str = field(default="Unification Sieve bottom")

@dataclass(repr=False)
class AcabUnifyVariableInconsistencyException(AcabTypingException):
    msg : str = field(default="Inconsistent Variable Found")

    def __str__(self):
        return f"{self.msg} : {self.left} : {self.right}"

@dataclass(repr=False)
class TypeRedefinitionException(AcabTypingException):
    msg : str = field(default="Type Redefinition Attempt")

@dataclass(repr=False)
class TypeConflictException(AcabTypingException):
    msg : str = field(default=f"Type Conflict: Not a subtype")

    def __str__(self):
        return f"{self.msg} : {self.left} : {self.right}"

@dataclass(repr=False)
class TypeUndefinedException(AcabTypingException):
    msg : str = field(default="Exception: Attempted to declare as missing type")

@dataclass(repr=False)
class TypeStructureMismatch(AcabTypingException):
    msg : str = field(default="Type Structure Mismatch")


@dataclass(repr=False)
class AcabUnifyFailure(AcabTypingException):
    msg : str = field(default="Unification Failure")

    def __str__(self):
        return f"{self.msg}: {self.left} : {self.right}"
