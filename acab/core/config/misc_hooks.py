#!/usr/bin/env python3
from __future__ import annotations
from typing import Tuple, Any
from typing import Callable, Iterator, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic, TypeAlias
from typing import TYPE_CHECKING, Protocol, TypeGuard
from typing import Final, final, overload, runtime_checkable
import abc
from dataclasses import dataclass, field, InitVar

if TYPE_CHECKING:
    # tc only imports
    pass


def attr_hook(self):
    """
    Config Hook to Generate attr access to data
    """
    self.attr._generate()


def import_hook(self):
    """
    Config Hook to import and bind targetted imports
    """
    # TODO get [Imports.Targeted]
    # For each key, import the value up to the last \.,
    # then getattr the last name from the module,
    # and override the key with it
    pass

def pyparsing_hook(self):
    packrat_spec = self.prepare("Parse", "PP_PACKRAT", _type=bool)
    if self.value(packrat_spec):
        import pyparsing as pp
        pp.ParserElement.enable_packrat()

    debug_spec = self.prepare("Parse", "DEBUG_PARSERS", _type=bool)
    if self.value(debug_spec):
        import acab.core.parsing.debug_funcs as DBF
        DBF.debug_pyparsing()
