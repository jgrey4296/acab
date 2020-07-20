import pyparsing as pp
import logging as root_logger

from acab.modules.analysis.typing.values.type_definition import TypeDefinition, SumTypeDefinition
from acab.modules.analysis.typing.values.operator_definition import OperatorDefinition
from acab.abstract.parsing import util as PU
from acab.abstract.sentence import Sentence
from acab.modules.analysis.typing import util as TYU
from acab import util
from . import util as TU
logging = root_logger.getLogger(__name__)

#Hotloaded definitions:
## Basic sentence (unable to parse annotations)
HOTLOAD_BASIC_SEN = pp.Forward()
## Param sentence (able to parse annotations)
HOTLOAD_PARAM_SEN = pp.Forward()

PARAM_SEN_PLURAL = pp.delimitedList(HOTLOAD_PARAM_SEN, delim=PU.DELIM)

def make_record_def(toks):
    type_def = TypeDefinition(toks[:])
    return (util.VALUE_TYPE_S, type_def)

def make_op_def(toks):
    syntax_bind = None
    if TYU.SYNTAX_BIND_S in toks:
        syntax_bind = toks[TYU.SYNTAX_BIND_S][0]

    op_def = OperatorDefinition(toks[TYU.STRUCT_S][0], sugar_syntax=syntax_bind)

    return (util.VALUE_TYPE_S, op_def)

def make_sum_def(toks):
    sum_def = SumTypeDefinition(toks[:])
    return (util.VALUE_TYPE_S, sum_def)


# The simplest type, has no body. useful for defining strings and other
# primitives
SIMPLE_BODY = pp.empty
SIMPLE_BODY.setParseAction(make_record_def)

SIMPLE_DEF = PU.STATEMENT_CONSTRUCTOR(PU.STRUCT_HEAD,
                                      HOTLOAD_BASIC_SEN,
                                      SIMPLE_BODY)

# Record Type definition:
# a.test.type: (::σ)  a.value.$x(::num) end
RECORD_DEF_BODY = PARAM_SEN_PLURAL
RECORD_DEF_BODY.setParseAction(make_record_def)

RECORD_TYPE = PU.STATEMENT_CONSTRUCTOR(PU.STRUCT_HEAD,
                                       HOTLOAD_BASIC_SEN,
                                       RECORD_DEF_BODY + PU.component_gap)

# Sum Type definition
# ie: first subwords are the subtypes. subtypes are automatic records
SUM_DEF_BODY = pp.delimitedList(pp.Or([RECORD_TYPE, SIMPLE_DEF]), delim=PU.emptyLine)
SUM_DEF_BODY.setParseAction(make_sum_def)

SUM_TYPE = PU.STATEMENT_CONSTRUCTOR(PU.SUM_HEAD,
                                    HOTLOAD_BASIC_SEN,
                                    SUM_DEF_BODY + PU.component_gap)



# numAdd: (::λ) $x(::num).$y(::num).$z(::num) => +
OP_DEF_BODY = PU.NG(TYU.STRUCT_S, HOTLOAD_PARAM_SEN) \
    + PU.op(PU.DBLARROW + PU.N(TYU.SYNTAX_BIND_S, PU.OPERATOR_SUGAR))
OP_DEF_BODY.setParseAction(make_op_def)

OP_DEF = PU.STATEMENT_CONSTRUCTOR(PU.FUNC_HEAD,
                                  HOTLOAD_BASIC_SEN,
                                  OP_DEF_BODY,
                                  end=pp.lineEnd,
                                  single_line=True)

# Type class constructor:
TYPE_CLASS_BODY = pp.delimitedList(OP_DEF, delim=PU.emptyLine)

TYPE_CLASS_DEF = PU.STATEMENT_CONSTRUCTOR(PU.TYPE_CLASS_HEAD,
                                          HOTLOAD_BASIC_SEN,
                                          TYPE_CLASS_BODY + PU.component_gap)


COMBINED_DEFS = pp.Or([SUM_TYPE, RECORD_TYPE, OP_DEF, SIMPLE_DEF])

# NAMING
RECORD_DEF_BODY.setName("TypeDefinitionBody")
RECORD_TYPE.setName("TypeDefinition")
OP_DEF_BODY.setName("OperatorDefinitionBody")
OP_DEF.setName("OperatorDefinition")

# parse_point = COMBINED_DEFS.ignore(PU.COMMENT)
parse_point = COMBINED_DEFS

def parseString(in_string):
    return parse_point.parseString(in_string)
