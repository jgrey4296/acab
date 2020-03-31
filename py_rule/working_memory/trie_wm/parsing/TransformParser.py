""" Trie-based parser for the transform component of rules """
import logging as root_logger
import pyparsing as pp
from py_rule.abstract.transform import TransformComponent
from py_rule.abstract.transform import Transform, TransformOp
from py_rule.abstract.parsing import util as PU
from py_rule.working_memory.trie_wm import util as WMU
from py_rule.working_memory.trie_wm.parsing.FactParser import VALBIND, BASIC_SEN

logging = root_logger.getLogger(__name__)


# Builders:
def build_operators():
    OP_SUGAR = [PU.OPERATOR_SUGAR]

    if BINARY_TRANS_OP.expr is not None:
        logging.debug("Transform Binary Operator Overwrite")
    BINARY_TRANS_OP << pp.Or([pp.Literal(k) for k, v
                              in TransformOp.op_list[2].items()] + OP_SUGAR)

    if UNARY_TRANS_OP.expr is not None:
        logging.debug("Transform Unary Operator Overwrite")
    UNARY_TRANS_OP << pp.Or([pp.Literal(k) for k, v
                             in TransformOp.op_list[1].items()] + OP_SUGAR)

    if TERNARY_TRANS_OP.expr is not None:
        logging.debug("Transform Ternary Operator Overwrite")
    TERNARY_TRANS_OP << pp.Or([pp.Literal(k) for k, v
                               in TransformOp.op_list[3].items()] + OP_SUGAR)


def buildBinaryTransformComponent(toks):
    return TransformComponent(toks[WMU.OPERATOR_S],
                             [toks[WMU.LEFT_S],
                              toks[WMU.RIGHT_S]])


def buildUnaryTransformComponent(toks):
    return TransformComponent(toks[WMU.OPERATOR_S],
                             [toks[WMU.RIGHT_S]])


def buildTernaryTransformComponent(toks):
    return TransformComponent(toks[WMU.OPERATOR_S],
                             [toks[WMU.SOURCE_S],
                              toks[WMU.REGEX_S],
                              toks[WMU.REPLACE_S]])


def addRebind(toks):
    if WMU.TARGET_S in toks:
        toks[WMU.TRANSFORM_S][0].set_rebind(toks[WMU.TARGET_S][0])
    return toks[WMU.TRANSFORM_S][0]


def build_transform(toks):
    trans = Transform(toks[:])
    return (trans._type, trans)

# Hotloaded Transform Operators
BINARY_TRANS_OP = pp.Forward()
UNARY_TRANS_OP = pp.Forward()
TERNARY_TRANS_OP = pp.Forward()

rebind = PU.ARROW + VALBIND

# transform: ( bind op val|bind -> bind)
unary_transform_core = PU.N(WMU.OPERATOR_S, UNARY_TRANS_OP) \
    + PU.N(WMU.RIGHT_S, VALBIND)

binary_transform_core = PU.N(WMU.LEFT_S, VALBIND) \
    + PU.N(WMU.OPERATOR_S, BINARY_TRANS_OP) \
    + PU.N(WMU.RIGHT_S, VALBIND)

ternary_transform_core = PU.N(WMU.SOURCE_S, VALBIND) \
    + PU.N(WMU.OPERATOR_S, TERNARY_TRANS_OP) \
    + PU.N(WMU.REGEX_S, VALBIND) \
    + PU.N(WMU.REPLACE_S, VALBIND)

transform_core = PU.NG(WMU.TRANSFORM_S,
                       pp.Or([binary_transform_core,
                              ternary_transform_core,
                              unary_transform_core])) \
                              + PU.N(WMU.TARGET_S, rebind)

transforms = pp.delimitedList(transform_core, delim=PU.DELIM)

transform_statement = PU.STATEMENT_CONSTRUCTOR(PU.TRANSFORM_HEAD,
                                               BASIC_SEN,
                                               transforms)

# Actions
binary_transform_core.setParseAction(buildBinaryTransformComponent)
unary_transform_core.setParseAction(buildUnaryTransformComponent)
ternary_transform_core.setParseAction(buildTernaryTransformComponent)

transform_core.setParseAction(addRebind)
transforms.setParseAction(build_transform)

# NAMING
BINARY_TRANS_OP.setName("BinaryTransformOperator")
UNARY_TRANS_OP.setName("UnaryTransformOperator")
TERNARY_TRANS_OP.setName("TernaryTransformOperator")
rebind.setName("RebindStatement")
unary_transform_core.setName("UnaryTransform_CORE")
binary_transform_core.setName("BinaryTransform_CORE")
ternary_transform_core.setName("TernaryTransform_CORE")
transform_core.setName("Transform_CORE")
transforms.setName("TransformPlural")
transform_statement.setName("TransformDefinition")

# parse_point = transforms.ignore(PU.COMMENT)
parse_point = transforms

# Main Parser:
def parseString(in_string):
    return parse_point.parseString(in_string)[0][1]
