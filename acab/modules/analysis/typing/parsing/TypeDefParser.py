import logging as root_logger

import pyparsing as pp
from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import Sentence
from acab.abstract.parsing import funcs as Pfunc
from acab.abstract.parsing import parsers as PU
from acab.abstract.parsing.consts import (DBLARROW, DELIM, NG, N,
                                          component_gap, emptyLine, op)
from acab.modules.analysis.typing import util as TYU

from acab.abstract.parsing.indented_block import IndentedBlock

from . import util as TU

logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

#Hotloaded definitions:
## Basic sentence (unable to parse annotations)
HOTLOAD_SEN = pp.Forward()
## Param sentence (able to parse annotations)

# the simplest type
# a.simple.type(::τ)
SIMPLE_DEF = PU.PARAM_CORE(end=pp.Literal("::τ"))
SIMPLE_DEF.setParseAction(TU.make_simple_def)

# Record Type definition:
# a.test.type(::σ):  a.value.$x(::num) end
RECORD_DEF_BODY = IndentedBlock(HOTLOAD_SEN)
RECORD_DEF_BODY.setParseAction(TU.make_record_def)

RECORD_TYPE = PU.STATEMENT_CONSTRUCTOR(pp.Literal("::τ"), RECORD_DEF_BODY)

# Sum Type definition
# ie: first subwords are the subtypes. subtypes are automatically records
SUM_DEF_BODY = IndentedBlock(RECORD_TYPE | SIMPLE_DEF)
SUM_DEF_BODY.setParseAction(TU.make_sum_def)

SUM_TYPE = PU.STATEMENT_CONSTRUCTOR(pp.Literal("::Σ"), SUM_DEF_BODY )

# numAdd(::λ): $x(::num).$y(::num).$z(::num) => +
# TODO: enable alias paths, not just sugar
OP_DEF_BODY = PU.PARAM_CORE(end=op(DBLARROW + N(TYU.SYNTAX_BIND_S, PU.OPERATOR_SUGAR)))
OP_DEF_BODY.setParseAction(TU.make_op_def)

OP_DEF = PU.STATEMENT_CONSTRUCTOR(pp.Literal("::λ"),
                                  OP_DEF_BODY,
                                  end=pp.lineEnd,
                                  single_line=True)

# Type class constructor:
TYPE_CLASS_BODY = IndentedBlock(HOTLOAD_SEN)
TYPE_CLASS_BODY.setParseAction(TU.make_type_class)

TYPE_CLASS_DEF = PU.STATEMENT_CONSTRUCTOR(pp.Literal("::ι"), TYPE_CLASS_BODY)

COMBINED_DEFS = pp.MatchFirst([SUM_TYPE, RECORD_TYPE, OP_DEF, SIMPLE_DEF])

# NAMING
RECORD_DEF_BODY.setName("TypeDefinitionBody")
RECORD_TYPE.setName("TypeDefinition")
OP_DEF_BODY.setName("OperatorDefinitionBody")
OP_DEF.setName("OperatorDefinition")

# parse_point = COMBINED_DEFS.ignore(COMMENT)
parse_point = COMBINED_DEFS

def parseString(in_string):
    return parse_point.parseString(in_string)
