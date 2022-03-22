import logging as root_logger
import re
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import pyparsing as pp
from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.data import default_structure as CDS
from acab.core.data.sentence import Sentence
from acab.core.parsing import consts as PConst
from acab.core.parsing import default_keys as PDS
from acab.core.parsing import default_symbols as PDSYM
from acab.core.parsing import funcs as Pfunc
from acab.core.parsing.annotation import ModalAnnotation, ValueAnnotation
from acab.core.parsing.consts import (CPAR, DBLCOLON, NG, OPAR, TAG, N,
                                      component_gap, emptyLine, gap, ln, op,
                                      opLn, orm, s, s_key, s_lit, zrm)
from acab.core.parsing.indented_block import IndentedBlock
from acab.interfaces.value import ValueFactory_i

logging = root_logger.getLogger(__name__)

ParserElement = AT.Parser

config = AcabConfig()

Fwd_ArgList = pp.Forward()
Fwd_TagList = pp.Forward()

def DELIMIST(expr, delim=None, stopOn=None):
    # TODO deprecate
    dlName = f"[{expr} {delim}...]"
    return (expr + pp.ZeroOrMore(delim + expr,
                                 stopOn=stopOn)).set_name(dlName)

def PARAM_CORE(mid:None|ParserElement=None,
               end:None|ParserElement=None,
               req_mid:None|ParserElement=None):
    """ Construct a parameterised core parser
    Can handle wrapped annotations, and a modality as suffix

    Params:
    mid - standard optional params
    end - the way the param core ends. usually a modal or eol.
    req_mid - required paramsthat have to occur for this to match


    """
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

    parser = pp.Group( VALBIND + mid_p + end )
    parser.set_parse_action(Pfunc.add_annotations)
    return parser

def type_annotation_gen(parser:ParserElement) -> ParserElement:
    return pp.Literal(PDSYM.TYPE_SEN) + parser


def STATEMENT_CONSTRUCTOR(annotation_p:ParserElement,
                          body_p:ParserElement,
                          end:Tuple[None,bool,ParserElement]=None,
                          args:bool=True,
                          single_line:bool=False,
                          parse_fn:None|Callable=None):
    """ Construct a parser for statements of the form:
    a.location(annotation_p): |args| components end
    """
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

    head = PARAM_CORE(req_mid=type_annotation_p, end=PConst.COLON)

    parser = NG(PDS.NAME, head) + line_end_p \
        + op(arg_p + emptyLine) \
        + op(Fwd_TagList + emptyLine) \
        + NG(PDS.STATEMENT, body_p) + end_p


    if parse_fn is not None:
        parser.add_parse_action(parse_fn)
    else:
        parser.add_parse_action(Pfunc.construct_statement)

    return parser.ignore(PConst.COMMENT)


HOTLOAD_VALUES = pp.Forward()
HOTLOAD_HEAD_ANNOTATIONS = pp.Forward()
HOTLOAD_POST_ANNOTATIONS = pp.Forward()

# Basic Parsers
OPERATOR_SUGAR = pp.Word(PDSYM.OPERATOR_SYNTAX)
OPERATOR_SUGAR.set_parse_action(lambda s, l, t: ValueFactory_i.sen([t[0]]))

ATOM           = pp.Word(PDSYM.WORD_COMPONENT + "'")
ATOM.set_parse_action(lambda s, l, t: (CDS.TYPE_BOTTOM_NAME, t[0]))

STRING      = pp.dbl_quoted_string
# Remove quotes from around strings:
STRING.set_parse_action(pp.remove_quotes)
STRING.add_parse_action(lambda s, l, t: (CDS.STRING_PRIM, t[0]))

# TODO add re.RegexFlag 's to parser: g and i
REGEX       = pp.Regex(r'/.+?/')
REGEX.set_parse_action(lambda s, l, t: (CDS.REGEX_PRIM, re.compile(t[0][1:-1])))


# Generalised modal operator, which is converted to appropriate data later
# The syntax is constructed automatically from AcabConfig
MODAL      = pp.Word("".join(config.syntax_extension.keys()))
MODAL.set_parse_action(lambda s, l, t: ModalAnnotation(t[0]))

BASIC_VALUE = ATOM | STRING | REGEX
BIND        = s_lit(PDSYM.BIND).set_parse_action(lambda s,l,t: ValueAnnotation(CDS.BIND, True))
AT_BIND     = s_lit(PDSYM.AT_BIND).set_parse_action(lambda s,l,t: ValueAnnotation(CDS.BIND, CDS.AT_BIND))

HEAD_ANNOTATIONS = BIND | AT_BIND | HOTLOAD_HEAD_ANNOTATIONS
POST_ANNOTATIONS = HOTLOAD_POST_ANNOTATIONS

VALBIND = (NG(PDS.HEAD_ANNOTATION, zrm(HEAD_ANNOTATIONS))
           + NG(PDS.VALUE, BASIC_VALUE | HOTLOAD_VALUES)
           + NG(PDS.POST_ANNOTATION, zrm(POST_ANNOTATIONS)))
VALBIND.set_parse_action(Pfunc.make_value)

Fwd_ArgList <<= PConst.VBAR + DELIMIST(BIND, delim=PConst.COMMA) + PConst.VBAR

# TODO make TAG a head_annotation
tagSen = TAG + pp.delimited_list(ATOM, delim=".")
tagSen.set_parse_action(lambda s, l, t: (ValueFactory_i.sen([x[1] for x in t[:]])))

Fwd_TagList <<= IndentedBlock(tagSen)(PDS.TAG)

# NAMING
# HOTLOAD_VALUES.set_name("HotloadValues")
VALBIND.set_name("ValBind")
# ATOM.set_name("NameWord")
# STRING.set_name("StringWord")
# REGEX.set_name("RegexWord")
# BASIC_VALUE.set_name("BasicValue")
# BIND.set_name("Binding")
# AT_BIND.set_name("AtBinding")
tagSen.set_name("TagSentence")
Fwd_TagList.set_name("TagList")
Fwd_ArgList.set_name("ArgList")
