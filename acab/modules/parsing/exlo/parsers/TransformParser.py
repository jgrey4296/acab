""" Trie-based parser for the transform component of rules """
import logging as root_logger

import pyparsing as pp
from acab.abstract.config.config import AcabConfig
from acab.abstract.parsing import parsers as PU
from acab.abstract.parsing.consts import (ARROW, COLON, COMMA, DELIM,
                                          DOUBLEBAR, NG, N, component_gap, zrm)
from acab.abstract.parsing.default_symbols import TRANSFORM_HEAD
from acab.modules.parsing.exlo.constructors import (build_transform,
                                                    build_transform_component)
from acab.modules.parsing.exlo.util import (LEFT_S, OPERATOR_S, RIGHT_S,
                                            TARGET_S)

from .FactParser import BASIC_SEN, PARAM_SEN, op_path

logging = root_logger.getLogger(__name__)

# Hotloaded Transform Operators
HOTLOAD_TRANS_OP = pp.Forward()
HOTLOAD_TRANS_STATEMENTS = pp.Forward()

rebind = ARROW + PU.VALBIND


# TODO: extend transform to take partial transforms?
# transform: ( bind op val|bind -> bind)

vals = N(RIGHT_S, zrm(PARAM_SEN))
# vals.addCondition(lambda toks: all([isinstance(x, Sentence) for x in toks]))

transform_core = N(OPERATOR_S, op_path) \
    + vals \
    + N(TARGET_S, rebind)

transform_sugar = NG(LEFT_S, PARAM_SEN) \
    + N(OPERATOR_S, HOTLOAD_TRANS_OP) \
    + vals \
    + N(TARGET_S, rebind)

transform_combined = pp.MatchFirst([transform_core,
                                    HOTLOAD_TRANS_STATEMENTS,
                                    transform_sugar])

transforms = pp.delimitedList(transform_combined, delim=DELIM)

transform_statement = PU.STATEMENT_CONSTRUCTOR(BASIC_SEN, transforms)

# Actions
transform_core.setParseAction(build_transform_component)
transform_sugar.addParseAction(build_transform_component)
transforms.setParseAction(build_transform)

# NAMING
# transform_core.setName("Transform_CORE")
transforms.setName("Transform")
transform_statement.setName("TransformStatement")
# HOTLOAD_TRANS_OP.setName("Transform_Op")
# HOTLOAD_TRANS_STATEMENTS.setName("Transform_Statement")
# rebind.setName("Rebind")

parse_point = transforms

# Main Parser:
def parseString(in_string):
    return parse_point.parseString(in_string)[0][1]
