import pyparsing as pp
import logging as root_logger
from . import FactParser as FP
from py_rule.typing.type_definition import TypeDefinition
logging = root_logger.getLogger(__name__)

def make_type_def(toks):
    return TypeDefinition(toks.SEN[0][-1],
                          toks.SEN[0],
                          toks.Structure[:],
                          toks.TVars[:])

s = pp.Suppress
op = pp.Optional
comment = s(pp.dblSlashComment)
emptyLine = s(pp.OneOrMore(pp.lineEnd))

VARLIST = FP.OPAR + pp.delimitedList(FP.VALBIND, ', ', False) + FP.CPAR

TYPEDEF = FP.DBLCOLON + FP.NG("SEN", FP.basic_fact_string) \
    + FP.N("TVars", op(VARLIST)) + FP.COLON  + emptyLine \
    + FP.N("Structure", FP.param_fact_strings) + emptyLine \
    + s(pp.Literal("END"))

TYPEDEF.setParseAction(lambda t: make_type_def(t))

def parseString(in_string):
    return TYPEDEF.parseString(in_string)
