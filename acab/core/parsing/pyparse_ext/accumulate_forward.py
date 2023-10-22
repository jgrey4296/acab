#!/usr/bin/env python3
##-- imports
from __future__ import annotations

import abc
import logging as logmod
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

import pyparsing as pp

if TYPE_CHECKING:
    # tc only imports
    pass

##-- end imports

logging = logmod.getLogger(__name__)

class AccumulateForward(pp.Forward):
    """
    A Customized PyParsing Forward Declaration,
    which can accumulate definitions instead of overwrite,
    and warns on re-binding otherwise.

    Also does a basic contain check to not add duplicates
    """


    def __init__(self, accumulate=False):
        super().__init__()
        self.accumulate = accumulate

    def clear(self):
        """ Clear any bindings that the Forward contains """
        self.expr         = None
        self._defaultName = None

    def __contains__(self, other):
        if not isinstance(self.expr, pp.MatchFirst):
            return self.expr == other
        if isinstance(other, pp.ParseExpression):
            whole = other in self.expr.exprs
            parts = all([x in self.expr.exprs for x in other.exprs])
            return whole or parts

        return other in self.expr.exprs

    def __lshift__(self, other):
        if hasattr(self, "caller_frame"):
            del self.caller_frame
        if isinstance(other, str):
            other = self._literalStringClass(other)

        if not self.accumulate and self.expr is not None:
            warnings.warn("Re-binding Forward")

        if not self.accumulate or self.expr is None:
            self.expr = other
        elif other not in self:
            self.expr |= other
            self.streamline()
            # Enable updating of default name:
            self._defaultName = None
            logging.info("Accumulating in a Forward: %s", self)
        else:
            warnings.warn("Tried to add a parser to a Forward which it already had")

        self.mayIndexError = self.expr.mayIndexError
        self.mayReturnEmpty = self.expr.mayReturnEmpty
        self.set_whitespace_chars(
            self.expr.whiteChars, copy_defaults=self.expr.copyDefaultWhiteChars
        )
        self.skipWhitespace = self.expr.skipWhitespace
        self.saveAsList = self.expr.saveAsList
        self.ignoreExprs.extend(self.expr.ignoreExprs)
        self.lshift_line = traceback.extract_stack(limit=2)[-2]
        return self
