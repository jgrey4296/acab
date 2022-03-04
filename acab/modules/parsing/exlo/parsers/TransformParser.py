""" Trie-based parser for the transform component of rules """
import logging as root_logger

import pyparsing as pp
from acab.core.config.config import AcabConfig
from acab.core.parsing import parsers as PU
from acab.core.parsing.consts import (ARROW, COLON, COMMA, DELIM,
                                          DOUBLEBAR, NG, N, component_gap, zrm)
from acab.core.parsing.default_symbols import TRANSFORM_HEAD
from acab.modules.parsing.exlo.constructors import (build_transform,
                                                    build_transform_component)
from acab.modules.parsing.exlo.util import (LEFT_S, OPERATOR_S, RIGHT_S,
                                            TARGET_S, TRANSFORM_HEAD)
from acab.core.parsing.indented_block import IndentedBlock

from .FactParser import SENTENCE, op_sentence, SEN_NO_MODAL

logging = root_logger.getLogger(__name__)

# Hotloaded Transform Operators
HOTLOAD_TRANS_OP         = pp.Forward()
HOTLOAD_TRANS_STATEMENTS = pp.Forward()

rebind                   = ARROW + SEN_NO_MODAL
rebind.set_name("rebind")

# TODO: extend transform to take partial transforms?
# transform: ( bind op val|bind -> bind)

vals = N(RIGHT_S, zrm(SENTENCE))
# vals.add_condition(lambda toks: all([isinstance(x, Sentence) for x in toks]))

transform_core = N(OPERATOR_S, op_sentence) \
    + vals \
    + N(TARGET_S, rebind)

transform_sugar = NG(LEFT_S, SENTENCE) \
    + N(OPERATOR_S, HOTLOAD_TRANS_OP) \
    + vals \
    + N(TARGET_S, rebind)

transform_combined = pp.MatchFirst([HOTLOAD_TRANS_STATEMENTS,
                                    transform_core,
                                    transform_sugar])

transforms = IndentedBlock(transform_combined)

transform_statement = PU.STATEMENT_CONSTRUCTOR(TRANSFORM_HEAD,
                                               transforms)

# Actions
transform_core.set_parse_action(build_transform_component)
transform_sugar.add_parse_action(build_transform_component)
transforms.set_parse_action(build_transform)

# NAMING
# transform_core.set_name("Transform_CORE")
transforms.set_name("Transform")
transform_statement.set_name("TransformStatement")
# HOTLOAD_TRANS_OP.set_name("Transform_Op")
# HOTLOAD_TRANS_STATEMENTS.set_name("Transform_Statement")
# rebind.set_name("Rebind")

parse_point = transforms

# Main Parser:
def parse_string(in_string):
    return parse_point.parse_string(in_string)[0][1]
