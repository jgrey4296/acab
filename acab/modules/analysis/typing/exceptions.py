"""
Exception types raised by type checking
"""
#https://docs.python.org/3/library/dataclasses.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from acab import types as AT
from dataclasses import dataclass, field, InitVar
from acab.modules.analysis.typing.values.acab_type import TypeStatement
from acab.error.base import AcabException

@dataclass
class AcabTypingException(AcabException):
    pass

@dataclass
class AcabMiscTypingException(AcabException):
    msg  : str = field()
    data : Any = field(default=None)

    def __str__(self):
        return self.msg

@dataclass
class TypeRedefinitionException(AcabTypingException):

    typename : AT.Sentence = field()
    msg      : str        = field(default="Type Redefinition Attempt")

@dataclass
class TypeUnifyException(AcabTypingException):

    left  : AT.Sentence     = field()
    right : AT.Sentence     = field()
    words : Tuple[AT.Value] = field()
    gamma : AT.CtxIns       = field()

@dataclass
class TypeConflictException(AcabTypingException):

    left          : AT.Sentence                     = field()
    right         : AT.Sentence                     = field()
    types         : Tuple[AT.Sentence, AT.Sentence] = field()
    gamma         : AT.CtxIns                       = field()

    def __str__(self):
        return f"Type Conflict: {self.types[0]} is not a subtype of {self.types[1]} in ({self.left} __:__ {self.right})"


@dataclass
class TypeUndefinedException(AcabTypingException):

    def __init__(self, attempted_type, stmt):
        self.attempted_type = attempted_type
        self.stmt = stmt

    def __str__(self):
        return "Exception: Attempted to declare as missing type {} in {}".format(self.attempted_type,
                                                                                 self.stmt)

@dataclass
class TypeVariableConflictException(AcabTypingException):

    def __init__(self, node_path):
        self.node_path = node_path

    def __str__(self):
        return "Node specified as both a var and not a var: {}".format("".join(self.node_path))

@dataclass
class TypeStructureMismatch(AcabTypingException):

    def __init__(self, typename, conflicts):
        self.typename = typename
        self.conflicts = conflicts

    def __str__(self):
        return "{} Structure Mismatch: {}".format(str(self.typename),
                                                  ", ".join([str(x) for x in self.conflicts]))
