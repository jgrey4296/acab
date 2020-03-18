import pyparsing as pp
import logging as root_logger
from py_rule.modules.analysis.typing.type_definition import TypeDefinition
from py_rule.abstract.parsing import util as PU
from py_rule.modules.analysis.typing import util as TYU
from . import util as TU
logging = root_logger.getLogger(__name__)

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
    assert TYU.SEN_S in toks
    assert TYU.STRUCT_S in toks
    tvars = []
    if TYU.TVAR_S in toks:
        tvars = toks[TYU.TVAR_S][:]

    type_def = TypeDefinition(toks[TYU.SEN_S][0][-1],
                          toks[TYU.SEN_S][0],
                          toks[TYU.STRUCT_S][:],
                          tvars)
    return (TYU.TYPE_DEF_S, type_def)

def make_op_def(toks):
    op_def = OperatorDefinition(
        toks[TYU.SEN_S][0],
        toks[TYU.STRUCT_S][0],
        toks[TYU.SYNTAX_BIND_S][0]
    )

    return (TYU.OP_DEF_S, op_def)



VARLIST = PU.OPAR + pp.delimitedList(VALBIND, TYU.DELIM_S, False) + PU.CPAR

# σ::a.test.type: a.value.$x(::num) end
SIGMA = PU.s(pp.Literal('σ'))
TYPEDEF = SIGMA + PU.DBLCOLON + PU.NG(TYU.SEN_S, BASIC_SEN) \
    + PU.N(TYU.TVAR_S, PU.op(VARLIST)) + PU.COLON + PU.emptyLine \
    + PU.N(TYU.STRUCT_S, PARAM_SEN_PLURAL) + PU.emptyLine \
    + PU.end

# λ::numAdd: $x(::num).$y(::num).$z(::num) => +
LAMBDA = PU.s(pp.Literal('λ'))
OP_DEF = LAMBDA + PU.DBLCOLON + PU.NG(TYU.SEN_S, BASIC_SEN) \
    + PU.COLON + PU.NG(TYU.STRUCT_S, PARAM_SEN) \
    + PU.op(PU.DBLARROW + PU.N(TYU.SYNTAX_BIND_S, BASIC_SEN)) + pp.lineEnd

TYPEDEF.setParseAction(make_type_def)
OP_DEF.setParseAction(make_op_def)

COMBINED_DEFS = pp.Or(TYPEDEF, OP_DEF)

def parseString(in_string):
    return COMBINED_DEFS.parseString(in_string)
