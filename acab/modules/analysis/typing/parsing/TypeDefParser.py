"""
DSL Fragment describing:
a simple type description
sum and product type descriptions, as statement blocks
operator type description
type class description

"""
import logging as root_logger

import pyparsing as pp
from acab.core.config.config import AcabConfig
from acab.core.data.value import Sentence
from acab.core.parsing import funcs as Pfunc
from acab.core.parsing import parsers as PU
from acab.core.parsing.consts import (DBLARROW, DELIM, NG, N, component_gap,
                                      emptyLine, op)
from acab.core.parsing.indented_block import IndentedBlock
from acab.modules.analysis.typing import util as TYU

from . import util as TU

logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

#Hotloaded definitions:
## Basic sentence (unable to parse annotations)
HOTLOAD_SEN = pp.Forward()
## Param sentence (able to parse annotations)

# the simplest type
# a.simple.type(::τ)
nom_type_anno = PU.type_annotation_gen(TYU.NOM_HEAD)
NOMINAL_DEF = PU.PARAM_CORE(req_mid=nom_type_anno, end=True)
NOMINAL_DEF.addParseAction(TU.make_simple_def)

# Record Type definition:
# a block of sentences, describing structure
# a.test.type(::σ):  a.value.$x(::num) end
RECORD_DEF_BODY = IndentedBlock(HOTLOAD_SEN)
RECORD_DEF_BODY.setParseAction(TU.make_record_def)

RECORD_TYPE = PU.STATEMENT_CONSTRUCTOR(TYU.STRUCT_HEAD, RECORD_DEF_BODY)

# Sum Type definition
# ie: first subwords are the subtypes. subtypes are automatically records
SUM_DEF_BODY = IndentedBlock(HOTLOAD_SEN)
SUM_DEF_BODY.setParseAction(TU.make_sum_def)

SUM_TYPE = PU.STATEMENT_CONSTRUCTOR(TYU.SUM_HEAD, SUM_DEF_BODY )

# numAdd(::λ): $x(::num).$y(::num).$z(::num) => +
# TODO: enable alias paths, not just sugar
OP_SUGAR = DBLARROW + PU.OPERATOR_SUGAR(TYU.SYNTAX_BIND_S)
# TODO allow multi line defs.
# (ie: polymorphism)
OP_DEF_BODY = HOTLOAD_SEN("params") + op(OP_SUGAR)
OP_DEF_BODY.setParseAction(TU.make_op_def)

OP_DEF = PU.STATEMENT_CONSTRUCTOR(TYU.FUNC_HEAD,
                                  OP_DEF_BODY,
                                  end=pp.lineEnd,
                                  single_line=True)

# Type class constructor:
TYPE_CLASS_BODY = IndentedBlock(HOTLOAD_SEN)
TYPE_CLASS_BODY.setParseAction(TU.make_type_class)

TYPE_CLASS_DEF = PU.STATEMENT_CONSTRUCTOR(TYU.TYPE_CLASS_HEAD,
                                          TYPE_CLASS_BODY)

COMBINED_DEFS = SUM_TYPE | RECORD_TYPE | OP_DEF | NOMINAL_DEF

# NAMING
RECORD_DEF_BODY.setName("TypeDefinitionBody")
RECORD_TYPE.setName("TypeDefinition")
OP_DEF_BODY.setName("OperatorDefinitionBody")
OP_DEF.setName("OperatorDefinition")

# parse_point = COMBINED_DEFS.ignore(COMMENT)
parse_point = COMBINED_DEFS

def parseString(in_string):
    return parse_point.parseString(in_string)
