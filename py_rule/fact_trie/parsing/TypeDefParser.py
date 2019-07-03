import pyparsing as pp
import logging as root_logger
from . import FactParser as FP
from py_rule.typing.ex_types import TypeDefinition
logging = root_logger.getLogger(__name__)

def make_type_def(tokens):
    #TODO
    return TypeDefinition()


s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.lineEnd))
comment = s(pp.dblSlashComment)
OPAR = s(pp.Literal('('))
CPAR = s(pp.Litral(')')


VARLIST = OPAR + pp.delimtedList(FP.BIND, ', ', False) + CPAR

TYPEDEF = DBLCOLON + N("SEN", FP.basic_fact_string) \
    + N("TVars", op(VARLIST)) + COLON  + opLn \
    + N("Structure", FP.param_fact_strings)
    + s(pp.Keyword("END"))

TYPEDEF.setParseAction(lambda t: make_type_def(t))
