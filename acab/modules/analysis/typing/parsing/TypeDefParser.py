"""
DSL Fragment describing:
a simple type description
sum and product type descriptions, as statement blocks
operator type description
type class description

"""
import logging as logmod

import pyparsing as pp
from acab import AcabConfig
from acab.core.parsing import funcs as Pfunc
from acab.core.parsing import parsers as PU
from acab.core.parsing.consts import (DBLARROW, DELIM, NG, N, component_gap, ARROW,
                                      emptyLine, ln, op, s, s_key, orm, END)
from acab.core.parsing.pyparse_ext.param_core import ParamCore
from acab.core.parsing.pyparse_ext.statement_core import StatementCore, type_annotation_gen
from acab.core.defaults.semantic_signals import signals

from acab.core.value.sentence import Sentence

from . import util as TU

logging = logmod.getLogger(__name__)

config = AcabConfig()

#Hotloaded definitions:
## Basic sentence (unable to parse annotations)
HOTLOAD_SEN  = pp.Forward()
HOTLOAD_WORD = pp.Forward()
## Param sentence (able to parse annotations)

# the simplest type
# a.simple.type(::τ)
nom_type_anno = type_annotation_gen(TU.NOM_HEAD)
NOMINAL_DEF = ParamCore(req_mid=nom_type_anno, end=True)
NOMINAL_DEF.add_parse_action(TU.make_simple_def)

# Record Type definition:
# a block of sentences, describing structure
# a.test.type(::σ):  a.value.$x(::num) end
RECORD_DEF_BODY = pp.IndentedBlock(HOTLOAD_SEN + op(s(ln)))
RECORD_DEF_BODY.set_parse_action(TU.make_record_def)

RECORD_TYPE = StatementCore(TU.STRUCT_HEAD, RECORD_DEF_BODY)

# Sum Type definition
# ie: first subwords are the subtypes. subtypes are automatically records
SUM_DEF_BODY = pp.IndentedBlock(HOTLOAD_SEN + op(s(ln)))
SUM_DEF_BODY.set_parse_action(TU.make_sum_def)

SUM_TYPE = StatementCore(TU.SUM_HEAD, SUM_DEF_BODY )

# numAdd(::λ): $x(::num).$y(::num).$z(::num) => +
# TODO: enable alias paths, not just sugar
# TODO refactor OP_SUGAR as an annotations
OP_SUGAR = DBLARROW + PU.OPERATOR_SUGAR(TU.SYNTAX_BIND_S)
# TODO allow multi line defs. (ie: polymorphism)
OP_DEF_PARAMS = orm(HOTLOAD_SEN)("params") + op(ARROW + HOTLOAD_WORD("returns"))

SINGLE_OP_DEF_BODY = pp.Group(OP_DEF_PARAMS)
SINGLE_OP_DEF_BODY.set_parse_action(TU.make_op_def)

SINGLE_OP_DEF = StatementCore(TU.FUNC_HEAD,
                       SINGLE_OP_DEF_BODY,
                       end=pp.line_end,
                       single_line=True)

MULTI_OP_DEF_BODY = pp.IndentedBlock(pp.Group(OP_DEF_PARAMS + op(s(ln))))
MULTI_OP_DEF_BODY.set_parse_action(TU.make_op_def)

MULTI_OP_DEF = StatementCore(TU.FUNC_HEAD,
                             MULTI_OP_DEF_BODY)

OP_DEF = SINGLE_OP_DEF | MULTI_OP_DEF

# Type class constructor:
TYPE_CLASS_BODY = pp.IndentedBlock(HOTLOAD_SEN + op(s(ln)))
TYPE_CLASS_BODY.set_parse_action(TU.make_type_class)

TYPE_CLASS_DEF = StatementCore(TU.TYPE_CLASS_HEAD,
                               TYPE_CLASS_BODY)

COMBINED_DEFS = SUM_TYPE | RECORD_TYPE | OP_DEF | NOMINAL_DEF

# NAMING
RECORD_DEF_BODY.set_name("TypeDefinitionBody")
RECORD_TYPE.set_name("TypeDefinition")
SINGLE_OP_DEF_BODY.set_name("OperatorDefinitionBody")
MULTI_OP_DEF_BODY.set_name("OperatorDefinitionBody")
OP_DEF.set_name("OperatorDefinition")

# parse_point = COMBINED_DEFS.ignore(COMMENT)
parse_point = COMBINED_DEFS

def parse_string(in_string):
    return parse_point.parse_string(in_string)
