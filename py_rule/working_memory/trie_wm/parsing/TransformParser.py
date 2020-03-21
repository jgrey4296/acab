""" Trie-based parser for the transform component of rules """
import logging as root_logger
import pyparsing as pp
from py_rule.abstract.transform import OperatorTransform
from py_rule.abstract.transform import Transform, TransformOp
from py_rule.abstract.parsing import util as PU
from py_rule.working_memory.trie_wm import util as WMU
from py_rule.working_memory.trie_wm.parsing.FactParser import VALBIND

logging = root_logger.getLogger(__name__)


# Builders:
def build_operators():
    OP_SUGAR = [PU.OPERATOR_SUGAR]

    if BINARY_TRANS_OP.expr is not None:
        logging.warning("Transform Binary Operator Overwrite")
    BINARY_TRANS_OP << pp.Or([pp.Literal(k) for k, v
                              in TransformOp.op_list[2].items()] + OP_SUGAR)

    if UNARY_TRANS_OP.expr is not None:
        logging.warning("Transform Unary Operator Overwrite")
    UNARY_TRANS_OP << pp.Or([pp.Literal(k) for k, v
                             in TransformOp.op_list[1].items()] + OP_SUGAR)

    if TERNARY_TRANS_OP.expr is not None:
        logging.warning("Transform Ternary Operator Overwrite")
    TERNARY_TRANS_OP << pp.Or([pp.Literal(k) for k, v
                               in TransformOp.op_list[3].items()] + OP_SUGAR)


def buildBinaryTransformComponent(toks):
    return OperatorTransform(toks[WMU.OPERATOR_S],
                             (toks[WMU.LEFT_S],
                              toks[WMU.RIGHT_S]))


def buildUnaryTransformComponent(toks):
    return OperatorTransform(toks[WMU.OPERATOR_S],
                             tuple([toks[WMU.RIGHT_S]]))


def buildTernaryTransformComponent(toks):
    return OperatorTransform(toks[WMU.OPERATOR_S],
                             (toks[WMU.SOURCE_S],
                              toks[WMU.REGEX_S],
                              toks[WMU.REPLACE_S]))


def addRebind(toks):
    if WMU.TARGET_S in toks:
        toks[WMU.TRANSFORM_S][0].set_rebind(toks[WMU.TARGET_S][0])
    return toks[WMU.TRANSFORM_S][0]


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
                              + PU.op(PU.N(WMU.TARGET_S, rebind))

transforms = pp.delimitedList(transform_core, delim=PU.COMMA)

# Actions
binary_transform_core.setParseAction(buildBinaryTransformComponent)
unary_transform_core.setParseAction(buildUnaryTransformComponent)
ternary_transform_core.setParseAction(buildTernaryTransformComponent)

transform_core.setParseAction(addRebind)
transforms.setParseAction(lambda toks: Transform(toks[:]))


# Main Parser:
def parseString(in_string):
    return transforms.parseString(in_string)[0]
