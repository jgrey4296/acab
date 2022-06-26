#!/usr/bin/env python3
"""
Class based constructor for the core acab parser
"""

from __future__ import annotations
from typing import Tuple, Any
from typing import Callable, Iterator, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic, TypeAlias
from typing import TYPE_CHECKING, Protocol, TypeGuard
from typing import Final, final, overload, runtime_checkable
import abc
from dataclasses import dataclass, field, InitVar
import pyparsing as pp

if TYPE_CHECKING:
    # tc only imports
    pass


from acab.core.parsing import funcs as Pfunc
from acab.core.config.config import AcabConfig
from acab.core.parsing.consts import (CPAR, DBLCOLON, NG, OPAR, TAG, N,
                                      component_gap, emptyLine, gap, ln, op,
                                      opLn, orm, s, s_key, s_lit, zrm)
from acab.core.parsing.parsers import MODAL, VALBIND, SIMPLE_VALUE
config = AcabConfig()


class ParamCore(pp.ParseExpression):
    """ Construct a parameterised core parser
    Can handle wrapped annotations, and a modality as suffix

    Params:
    mid - standard optional params
    end - the way the param core ends. usually a modal or eol.
    req_mid - required paramsthat have to occur for this to match
    """

    def __init__(self, mid:None|ParserElement=None,
                 end:None|ParserElement=None,
                 req_mid:None|ParserElement=None,
                 simple=False):
        if mid is None and req_mid is None:
            mid_p = pp.Empty()
        elif mid is not None:
            mid_p = op(OPAR + mid + CPAR)
        else:
            assert(mid is None)
            mid_p = OPAR + req_mid + CPAR

        if end is None:
            end = MODAL
        elif not isinstance(end, pp.ParserElement):
            end = pp.Empty()

        base = VALBIND
        if simple:
            base = SIMPLE_VALUE

        parser = pp.Group(base + mid_p + end)
        parser.set_parse_action(Pfunc.add_annotations)
        super().__init__(parser)

    def parseImpl(self, instring, loc, doActions=True):
        loc, resultlist = self.exprs[0]._parse(instring,
                                               loc,
                                               doActions,
                                               callPreParse=False)
        return loc, resultlist
