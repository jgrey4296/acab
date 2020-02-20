import pyparsing as pp
import logging as root_logger
from py_rule.typing.type_definition import TypeDefinition
from py_rule.abstract.parsing import util as PU
from . import util as TU
logging = root_logger.getLogger(__name__)

SEN = "sen"
TVAR = "tvar"
STRUC = "structure"
DELIM = ", "

#Hotloaded definitions:
## Values / bindings
VALBIND = pp.Forward()
## Basic sentence (unable to parse annotations)
BASIC_SEN = pp.Forward()
## Param sentence (able to parse annotations)
PARAM_SEN = pp.Forward()

PARAM_SEN_PLURAL = PARAM_SEN \
    + pp.ZeroOrMore(PU.COMMA + PARAM_SEN)

def make_type_def(toks):
    assert SEN in toks
    assert STRUC in toks
    tvars = []
    if TVAR in toks:
        tvars = toks[TVAR][:]

    return TypeDefinition(toks[SEN][0][-1],
                          toks[SEN][0],
                          toks[STRUC][:],
                          tvars)


VARLIST = PU.OPAR + pp.delimitedList(VALBIND, DELIM, False) + PU.CPAR

TYPEDEF = PU.DBLCOLON + PU.NG(SEN, BASIC_SEN) \
    + PU.N(TVAR, PU.op(VARLIST)) + PU.COLON + PU.emptyLine \
    + PU.N(STRUC, PARAM_SEN_PLURAL) + PU.emptyLine \
    + PU.end

TYPEDEF.setParseAction(make_type_def)

def parseString(in_string):
    return TYPEDEF.parseString(in_string)
