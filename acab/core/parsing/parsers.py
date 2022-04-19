import logging as logmod
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
from acab.interfaces.value import ValueFactory_i

logging = logmod.getLogger(__name__)

ParserElement = AT.Parser

config = AcabConfig()

HOTLOAD_VALUES = pp.Forward()
HOTLOAD_VALUES.set_name('hotload_values')
HOTLOAD_HEAD_ANNOTATIONS = pp.Forward()
HOTLOAD_HEAD_ANNOTATIONS.set_name('hl_word_head_annot')
HOTLOAD_POST_ANNOTATIONS = pp.Forward()
HOTLOAD_POST_ANNOTATIONS.set_name('hl_word_post_annot')

Fwd_ArgList = pp.Forward()
Fwd_ArgList.set_name('fwd_arglist')
Fwd_TagList = pp.Forward()
Fwd_TagList.set_name('fwd_taglist')

def DELIMIST(expr, delim=None, stopOn=None):
    # TODO deprecate
    dlName = f"[{expr} {delim}...]"
    return (expr + pp.ZeroOrMore(delim + expr,
                                 stopOn=stopOn)).set_name(dlName)


# Basic Parsers
OPERATOR_SUGAR = pp.Word(PDSYM.OPERATOR_SYNTAX)
OPERATOR_SUGAR.set_parse_action(lambda s, l, t: ValueFactory_i.sen([t[0]]))

ATOM           = pp.Word(PDSYM.WORD_COMPONENT + "'")
ATOM.set_parse_action(lambda s, l, t: (CDS.TYPE_BASE, t[0]))

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

Fwd_ArgList <<= PConst.VBAR + DELIMIST(VALBIND, delim=PConst.COMMA) + PConst.VBAR

# TODO make TAG a head_annotation
tagSen = TAG + pp.delimited_list(ATOM, delim=".")
tagSen.set_parse_action(lambda s, l, t: (ValueFactory_i.sen([x[1] for x in t[:]])))

Fwd_TagList <<= pp.IndentedBlock(tagSen + zrm(pp.line_end.suppress() + tagSen))(PDS.TAG)

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
