import logging as root_logger
import re
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import pyparsing as pp
from acab import types as AT
from acab.abstract.config.config import AcabConfig
from acab.abstract.parsing.annotation import ModalAnnotation
from acab.abstract.core import default_structure as CDS
from acab.abstract.core.values import Sentence
from acab.abstract.parsing import consts as PConst
from acab.abstract.parsing import default_structure as PDS
from acab.abstract.parsing import default_symbols as PDSYM
from acab.abstract.parsing import funcs as Pfunc
from acab.abstract.parsing.consts import (CPAR, DBLCOLON, NG, OPAR, TAG, N,
                                          component_gap, emptyLine, gap, ln,
                                          op, opLn, orm, s, s_key, s_lit, zrm)
from acab.abstract.parsing.indented_block import IndentedBlock

logging = root_logger.getLogger(__name__)

ParserElement = AT.Parser

config = AcabConfig.Get()

Fwd_ArgList = pp.Forward()
Fwd_TagList = pp.Forward()

def DELIMIST(expr, delim=None, stopOn=None):
    dlName = f"[{expr} {delim}...]"
    return (expr + pp.ZeroOrMore(delim + expr,
                                 stopOn=stopOn)).setName(dlName)

def PARAM_CORE(mid:Optional[ParserElement]=None,
               end:Optional[ParserElement]=None):
    """ Construct a parameterised core parser
    Can handle wrapped annotations, and a modality as suffix
    """
    if mid is None:
        mid = pp.Empty()

    if end is None:
        end = MODAL(PDS.MODAL)
    elif not isinstance(end, pp.ParserElement):
        end = pp.Empty()

    parser = N(PDS.NODE, VALBIND) \
        + op(OPAR + mid + CPAR) + end
    parser.setParseAction(Pfunc.add_annotations)
    return parser




def STATEMENT_CONSTRUCTOR(annotation_p:ParserElement,
                          body_p:ParserElement,
                          end:Tuple[None,bool,ParserElement]=None,
                          args:bool=True,
                          single_line:bool=False,
                          parse_fn:Optional[Callable]=None):
    """ Construct a parser for statements of the form:
    a.location: |args| components end
    """
    line_p     = PConst.emptyLine
    line_end_p = PConst.ln
    end_p      = PConst.END
    arg_p      = pp.empty

    if single_line:
        line_p     = pp.empty
        line_end_p = pp.empty
        end_p      = pp.lineEnd
    elif end is not None:
        end_p = end

    if args:
        arg_p = Fwd_ArgList(PDS.ARG)

    head = PARAM_CORE(annotation_p, end=PConst.COLON)

    parser = NG(PDS.NAME, head) + line_end_p \
        + op(arg_p + emptyLine) \
        + op(Fwd_TagList + emptyLine) \
        + NG(PDS.STATEMENT, body_p) + end_p


    if parse_fn is not None:
        parser.addParseAction(parse_fn)
    else:
        parser.addParseAction(Pfunc.construct_statement)

    return parser.ignore(PConst.COMMENT)


HOTLOAD_VALUES = pp.Forward()


# Basic Parsers
OPERATOR_SUGAR = pp.Word(PDSYM.OPERATOR_SYNTAX)
OPERATOR_SUGAR.setParseAction(lambda s, l, t: Sentence.build([t[0]]))

# TODO use config for type sentences
ATOM           = pp.Word(PDSYM.WORD_COMPONENT)
ATOM.setParseAction(lambda s, l, t: (CDS.TYPE_BOTTOM_NAME, t[0]))

STRING      = pp.dblQuotedString
# Remove quotes from around strings:
STRING.setParseAction(pp.removeQuotes)
STRING.addParseAction(lambda toks: (CDS.STRING_PRIM, toks[0]))

# TODO add re.RegexFlag 's to parser: g and i
REGEX       = pp.Regex(r'/.+?/')
REGEX.setParseAction(lambda s, l, t: (CDS.REGEX_PRIM, re.compile(t[0][1:-1])))


# Generalised modal operator, which is converted to appropriate data later
# TODO refactor to be a ValueAnnotation
MODAL      = pp.Word("".join(config.syntax_extension.keys()))
MODAL.setParseAction(lambda s, l, t: ModalAnnotation(t[0]))

# TODO use valueannotations for this too
BASIC_VALUE = ATOM | STRING | REGEX
BIND        = s_lit(PDSYM.BIND) + ATOM
AT_BIND     = s_lit(PDSYM.AT_BIND) + ATOM

VALBIND = pp.MatchFirst([N(PDS.BIND, BIND),
                         N(PDS.AT_BIND, AT_BIND),
                         N(PDS.VALUE, BASIC_VALUE),
                         N(PDS.VALUE, HOTLOAD_VALUES)])
VALBIND.setParseAction(Pfunc.make_value)

Fwd_ArgList <<= PConst.VBAR + DELIMIST(BIND, delim=PConst.COMMA) + PConst.VBAR

tagSen = TAG + pp.delimitedList(ATOM, delim=".")
tagSen.setParseAction(lambda s, l, t: (Sentence.build([x[1] for x in t[:]])))

Fwd_TagList <<= IndentedBlock(tagSen)(PDS.TAG)

# NAMING
# HOTLOAD_VALUES.setName("HotloadValues")
VALBIND.setName("ValBind")
# ATOM.setName("NameWord")
# STRING.setName("StringWord")
# REGEX.setName("RegexWord")
# BASIC_VALUE.setName("BasicValue")
# BIND.setName("Binding")
# AT_BIND.setName("AtBinding")
tagSen.setName("TagSentence")
Fwd_TagList.setName("TagList")
Fwd_ArgList.setName("ArgList")
