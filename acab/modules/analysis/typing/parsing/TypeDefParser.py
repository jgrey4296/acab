import logging as root_logger

import pyparsing as pp
from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import Sentence
from acab.abstract.parsing import funcs as Pfunc
from acab.abstract.parsing import parsers as PU
from acab.abstract.parsing.consts import (DBLARROW, DELIM, NG, N,
                                          component_gap, emptyLine, op)
from acab.modules.analysis.typing import util as TYU
from acab.modules.analysis.typing.values.operator_definition import \
    OperatorDefinition
from acab.modules.analysis.typing.values.type_definition import (
    SumTypeDefinition, TypeDefinition)

from acab.abstract.parsing.indented_block import IndentedBlock

from . import util as TU

logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

#Hotloaded definitions:
## Basic sentence (unable to parse annotations)
HOTLOAD_BASIC_SEN = pp.Forward()
## Param sentence (able to parse annotations)
HOTLOAD_PARAM_SEN = pp.Forward()

# The simplest type, has no body. useful for defining strings and other
# primitives
SIMPLE_BODY = pp.empty
SIMPLE_BODY.setParseAction(TU.make_record_def)

SIMPLE_DEF = PU.STATEMENT_CONSTRUCTOR(HOTLOAD_BASIC_SEN, SIMPLE_BODY)

# Record Type definition:
# a.test.type: (::σ)  a.value.$x(::num) end
RECORD_DEF_BODY = IndentedBlock(HOTLOAD_PARAM_SEN)
RECORD_DEF_BODY.setParseAction(TU.make_record_def)

RECORD_TYPE = PU.STATEMENT_CONSTRUCTOR(HOTLOAD_BASIC_SEN, RECORD_DEF_BODY)

# Sum Type definition
# ie: first subwords are the subtypes. subtypes are automatic records
SUM_DEF_BODY = IndentedBlock(pp.Or([RECORD_TYPE, SIMPLE_DEF]))
SUM_DEF_BODY.setParseAction(TU.make_sum_def)

SUM_TYPE = PU.STATEMENT_CONSTRUCTOR(HOTLOAD_BASIC_SEN, SUM_DEF_BODY )

# numAdd: (::λ) $x(::num).$y(::num).$z(::num) => +
# TODO: enable alias paths, not just sugar
OP_DEF_BODY = NG(TYU.STRUCT_S, HOTLOAD_PARAM_SEN) \
    + op(DBLARROW + N(TYU.SYNTAX_BIND_S, PU.OPERATOR_SUGAR))
OP_DEF_BODY.setParseAction(TU.make_op_def)

OP_DEF = PU.STATEMENT_CONSTRUCTOR(HOTLOAD_BASIC_SEN,
                                  OP_DEF_BODY,
                                  end=pp.lineEnd,
                                  single_line=True)

# Type class constructor:
TYPE_CLASS_BODY = IndentedBlock(OP_DEF)

TYPE_CLASS_DEF = PU.STATEMENT_CONSTRUCTOR(HOTLOAD_BASIC_SEN, TYPE_CLASS_BODY)

COMBINED_DEFS = pp.Or([SUM_TYPE, RECORD_TYPE, OP_DEF, SIMPLE_DEF])

# NAMING
RECORD_DEF_BODY.setName("TypeDefinitionBody")
RECORD_TYPE.setName("TypeDefinition")
OP_DEF_BODY.setName("OperatorDefinitionBody")
OP_DEF.setName("OperatorDefinition")

# parse_point = COMBINED_DEFS.ignore(COMMENT)
parse_point = COMBINED_DEFS

def parseString(in_string):
    return parse_point.parseString(in_string)
