#!/usr/bin/env python3
"""
Class Based Constructor for the core statement parser
"""
from __future__ import annotations

import abc
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

import pyparsing as pp

if TYPE_CHECKING:
    # tc only imports
    pass

from acab import AcabConfig
from acab.core.config.config import AcabConfig
from acab.core.defaults import parse_keys as PDS
from acab.core.defaults import parse_symbols as PDSYM
from acab.core.parsing import consts as PConst
from acab.core.parsing import funcs as Pfunc
from acab.core.parsing.consts import (CPAR, DBLCOLON, NG, OPAR, TAG, N,
                                      component_gap, emptyLine, gap, ln, op,
                                      opLn, orm, s, s_key, s_lit, zrm)
from acab.core.parsing.param_core import ParamCore
from acab.core.parsing.parsers import Fwd_ArgList, Fwd_TagList

config               = AcabConfig()
aliases              = config.attr.Aliases
type_annotate_prefix = pp.Literal(PDSYM.TYPE_SEN)

def type_annotation_gen(name:str|pp.ParseExpression) -> ParserElement:
    """
    Create a type annotation parser, using the config defined TYPE_SEN symbol,
    and the passed in string representing a semantic signal.
    if theres a defined alias for the signal, use that as an alternative
    """
    annotation = name
    if isinstance(annotation, str):
        annotation = pp.Literal(annotation)
    if name in aliases:
        annotation = annotation | pp.Literal(aliases[name])

    return type_annotate_prefix + annotation




class StatementCore(pp.ParseExpression):
    """ Construct a parser for statements of the form:
        a.location(annotation_p):\n |args|\n\n #tags\n\n { body }\n end
    """

    def __init__(self, annotation_p:str|pp.ParseExpression,
                 body_p:ParserElement,
                 end:Tuple[None,bool,ParserElement]=None,
                 args:bool=True,
                 single_line:bool=False,
                 parse_fn:None|Callable=None):

        line_p            = PConst.emptyLine
        line_end_p        = PConst.ln
        end_p             = PConst.END
        arg_p             = pp.empty
        type_annotation_p = type_annotation_gen(annotation_p)


        if single_line:
            line_p     = pp.empty
            line_end_p = pp.empty
            end_p      = pp.line_end
        elif end is not None:
            end_p = end

        if args:
            arg_p = Fwd_ArgList(PDS.ARG)

        head = ParamCore(req_mid=type_annotation_p, end=PConst.COLON, simple=True)
        parser = (NG(PDS.NAME, head) + line_end_p
                + op(arg_p + emptyLine)
                + op(Fwd_TagList + emptyLine)
                + NG(PDS.STATEMENT, body_p)
                + end_p)
        parser.streamline()

        if parse_fn is not None:
            parser.add_parse_action(parse_fn)
        else:
            parser.add_parse_action(Pfunc.construct_statement)
        super().__init__(parser.ignore(PConst.COMMENT))

    def parseImpl(self, instring, loc, doActions=True):
        loc, resultlist = self.exprs[0]._parse(instring,
                                               loc,
                                               doActions,
                                               callPreParse=False)
        return loc, resultlist
