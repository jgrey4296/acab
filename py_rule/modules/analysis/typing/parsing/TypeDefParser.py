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
    assert TYU.SEN_S in toks
    assert TYU.STRUCT_S in toks
    tvars = []
    if TYU.TVAR_S in toks:
        tvars = toks[TYU.TVAR_S][:]

    type_def = TypeDefinition(toks[TYU.SEN_S][0],
                              toks[TYU.STRUCT_S][:],
                              tvars)
    return (TYU.TYPE_DEF_S, type_def)

def make_op_def(toks):
    syntax_bind = None
    if TYU.SYNTAX_BIND_S in toks:
        syntax_bind = toks[TYU.SYNTAX_BIND_S][0]

    op_def = OperatorDefinition(toks[TYU.SEN_S][0],
                                toks[TYU.STRUCT_S][0],
                                syntax_bind)

    return (TYU.OP_DEF_S, op_def)



VARLIST = PU.OPAR + pp.delimitedList(HOTLOAD_VALBIND, TYU.DELIM_S, False) + PU.CPAR

# σ::a.test.type: a.value.$x(::num) end
TYPEDEF = PU.STRUCT_HEAD + PU.DBLCOLON + PU.NG(TYU.SEN_S, HOTLOAD_BASIC_SEN) \
    + PU.N(TYU.TVAR_S, PU.op(VARLIST)) + PU.COLON + PU.emptyLine \
    + PU.N(TYU.STRUCT_S, PARAM_SEN_PLURAL) + PU.emptyLine \
    + PU.end

# λ::numAdd: $x(::num).$y(::num).$z(::num) => +
OP_DEF = PU.FUNC_HEAD + PU.DBLCOLON + PU.NG(TYU.SEN_S, HOTLOAD_BASIC_SEN) \
    + PU.COLON + PU.NG(TYU.STRUCT_S, HOTLOAD_PARAM_SEN) \
    + PU.op(PU.DBLARROW + PU.N(TYU.SYNTAX_BIND_S, PU.OPERATOR_SUGAR)) + pp.lineEnd

TYPEDEF.setParseAction(make_type_def)
OP_DEF.setParseAction(make_op_def)

COMBINED_DEFS = pp.Or(TYPEDEF, OP_DEF)

def parseString(in_string):
    return COMBINED_DEFS.parseString(in_string)
