#!/usr/bin/env python3
import logging as root_logger
import pyparsing as pp
import re

from acab.abstract.config.config import AcabConfig

from acab.abstract.config.modal import SYNTAX_LOOKUP
from acab.abstract.parsing import funcs as Pfunc
from acab.abstract.parsing import consts as PConst
from acab.abstract.parsing.consts import emptyLine, s, op, orm, zrm, N, NG, MODAL_S, NODE_S
from acab.abstract.parsing.consts import gap, component_gap, OPAR, CPAR, DBLCOLON

logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()

Fwd_ArgList = pp.Forward()
Fwd_TagList = pp.Forward()

def PARAM_CORE(mid=None, end=None):
    """ Construct a parameterised core parser
    Can handle wrapped annotations, and a modality as suffix
    """
    if mid is None:
        mid = pp.Empty()
    if end is None:
        end = NG(MODAL_S, MODAL)
    else:
        end = pp.Empty()
    parser = N(NODE_S, VALBIND) \
        + op(OPAR + NG(PConst.ANNOTATION_S, mid) + CPAR) + end
    parser.setParseAction(Pfunc.add_annotations)
    return parser




def STATEMENT_CONSTRUCTOR(head_p,
                          name_p,
                          body_p,
                          end=None,
                          args=True,
                          single_line=False,
                          parse_fn=None):
    """ Construct statements of the form:
    a.location: (::λ) |args| components end
    """
    line_p = PConst.emptyLine
    end_p  = PConst.END
    arg_p  = pp.empty

    if single_line:
        line_p = pp.empty
        end_p = pp.lineEnd
    elif end is not None:
        end_p = end

    if args:
        arg_p = op(NG(PConst.ARG_S, Fwd_ArgList + line_p))

    head_hint = OPAR + DBLCOLON + head_p + CPAR

    parser = NG(PConst.NAME_S, name_p) + PConst.COLON + s(head_hint) + op(pp.lineEnd) \
        + arg_p + Fwd_TagList + NG(PConst.STATEMENT_S, body_p) + end_p

    if parse_fn is not None:
        parser.addParseAction(parse_fn)
    else:
        parser.addParseAction(Pfunc.construct_statement)
    return parser



def OP_PATH_C(sen):
    op_path = PConst.FUNC_SYMBOL + sen
    # op_path.setName("Operator_Path")
    return op_path

HOTLOAD_VALUES = pp.Forward()
HOTLOAD_VALUES.setName("HotloadValues")


# Basic Parsers
OPERATOR_SUGAR = pp.Word(PConst.OPERATOR_SYNTAX)

# TODO use config for type sentences
ATOM           = pp.Word(PConst.WORD_COMPONENT_S)
ATOM.setParseAction(lambda t: (PConst.ATOM_V, t[0]))

STRING      = pp.dblQuotedString
# Remove quotes from around strings:
STRING.setParseAction(pp.removeQuotes)
STRING.addParseAction(lambda toks: (PConst.STRING_V, toks[0]))

# TODO add re.RegexFlag 's to parser: g and i
REGEX       = pp.Regex(r'/.+?/')
REGEX.setParseAction(lambda t: (PConst.REGEX_V, re.compile(t[0][1:-1])))


# Generalised modal operator, which is converted to appropriate data later
MODAL      = pp.Word("".join(SYNTAX_LOOKUP.keys()))
MODAL.setParseAction(lambda t: (PConst.MODAL_S, SYNTAX_LOOKUP[t[0]]))

BASIC_VALUE = pp.Or([ATOM, STRING, REGEX])
BIND        = PConst.BIND_SYMBOL + ATOM
AT_BIND     = PConst.AT_BIND_SYMBOL + ATOM

VALBIND = pp.Or([N(PConst.BIND_S, BIND),
                 N(PConst.AT_BIND_S, AT_BIND),
                 N(PConst.VALUE_S, pp.Or([BASIC_VALUE, HOTLOAD_VALUES]))])
VALBIND.setParseAction(Pfunc.make_value)

Fwd_ArgList <<= PConst.VBAR + pp.delimitedList(BIND, delim=PConst.COMMA) + PConst.VBAR

tagName = PConst.TAG_SYMBOL + ATOM

Fwd_TagList <<= op(N(PConst.TAG_S,
                     pp.delimitedList(tagName, delim=PConst.DELIM)
                     + emptyLine))

# NAMING
VALBIND.setName("ValBind")
ATOM.setName("NameWord")
STRING.setName("StringWord")
REGEX.setName("RegexWord")
BASIC_VALUE.setName("BasicValue")
BIND.setName("Binding")
AT_BIND.setName("AtBinding")
tagName.setName("TagName")
Fwd_TagList.setName("StatementTagList")
Fwd_ArgList.setName("StatementArgumentList")
