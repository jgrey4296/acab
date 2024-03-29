#!/usr/bin/env python3
"""
Class based constructor for the core acab parser
"""
##-- imports
from __future__ import annotations

import abc
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

import pyparsing as pp
import acab
from acab.core.parsing import funcs as Pfunc
from acab.core.parsing.consts import (CPAR, DBLCOLON, NG, OPAR, TAG, N,
                                      component_gap, emptyLine, gap, ln, op,
                                      opLn, orm, s, s_key, s_lit, zrm)
from acab.core.parsing.parsers import MODAL, SIMPLE_VALUE, VALBIND

if TYPE_CHECKING:
    # tc only imports
    pass

##-- end imports

config = acab.config


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
