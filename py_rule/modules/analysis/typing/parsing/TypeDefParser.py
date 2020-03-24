import pyparsing as pp
import logging as root_logger
from py_rule.modules.analysis.typing.type_definition import TypeDefinition
from py_rule.modules.analysis.typing.operator_definition import OperatorDefinition
from py_rule.abstract.parsing import util as PU
from py_rule.modules.analysis.typing import util as TYU
from . import util as TU
logging = root_logger.getLogger(__name__)

#Hotloaded definitions:
## Values / bindings
HOTLOAD_VALBIND = pp.Forward()
## Basic sentence (unable to parse annotations)
HOTLOAD_BASIC_SEN = pp.Forward()
## Param sentence (able to parse annotations)
HOTLOAD_PARAM_SEN = pp.Forward()

PARAM_SEN_PLURAL = HOTLOAD_PARAM_SEN \
    + pp.ZeroOrMore(PU.COMMA + HOTLOAD_PARAM_SEN)

def make_type_def(toks):
    type_def = TypeDefinition(toks[:])
    return (type_def._type, type_def)

def make_op_def(toks):
    syntax_bind = None
    if TYU.SYNTAX_BIND_S in toks:
        syntax_bind = toks[TYU.SYNTAX_BIND_S][0]

    op_def = OperatorDefinition(toks[TYU.STRUCT_S][0],
                                syntax_bind)

    return (op_def._type, op_def)



VARLIST = PU.OPAR + pp.delimitedList(HOTLOAD_VALBIND, TYU.DELIM_S, False) + PU.CPAR

# σ::a.test.type: a.value.$x(::num) end
TYPEDEF_BODY = PARAM_SEN_PLURAL
TYPEDEF_BODY.setParseAction(make_type_def)

TYPEDEF = PU.STATEMENT_CONSTRUCTOR(PU.STRUCT_HEAD, HOTLOAD_BASIC_SEN, TYPEDEF_BODY)

# λ::numAdd: $x(::num).$y(::num).$z(::num) => +
OP_DEF_BODY = PU.NG(TYU.STRUCT_S, HOTLOAD_PARAM_SEN) \
    + PU.op(PU.DBLARROW + PU.N(TYU.SYNTAX_BIND_S, PU.OPERATOR_SUGAR))
OP_DEF_BODY.setParseAction(make_op_def)

OP_DEF = PU.STATEMENT_CONSTRUCTOR(PU.OP_HEAD, HOTLOAD_BASIC_SEN, OP_DEF_BODY, end=pp.lineEnd, single_line=True)


COMBINED_DEFS = pp.Or(TYPEDEF, OP_DEF)

def parseString(in_string):
    return COMBINED_DEFS.parseString(in_string)
