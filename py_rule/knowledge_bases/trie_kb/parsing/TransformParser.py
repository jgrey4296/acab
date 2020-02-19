""" Trie-based parser for the transform component of rules """
import logging as root_logger
import pyparsing as pp
from py_rule.abstract.transform import OperatorTransform
from py_rule.abstract.transform import Transform, TransformOp
from py_rule.abstract.parsing import util as PU
from py_rule.knowledge_bases.trie_kb import util as KBU
from py_rule.knowledge_bases.trie_kb.parsing.FactParser import VALBIND

logging = root_logger.getLogger(__name__)


# Builders:
def build_operators():
    if BINARY_TRANS_OP.expr is None:
        BINARY_TRANS_OP << pp.Or([pp.Literal(k) for k, v
                                  in TransformOp.op_list.items() if 2 in v])
    if UNARY_TRANS_OP.expr is None:
        UNARY_TRANS_OP << pp.Or([pp.Literal(k) for k, v
                                 in TransformOp.op_list.items() if 1 in v])
    if TERNARY_TRANS_OP.expr is None:
        TERNARY_TRANS_OP << pp.Or([pp.Literal(k) for k, v
                                   in TransformOp.op_list.items() if 3 in v])


def buildBinaryTransformComponent(toks):
    return OperatorTransform(toks[KBU.OPERATOR_S],
                             (toks[KBU.LEFT_S],
                              toks[KBU.RIGHT_S]))


def buildUnaryTransformComponent(toks):
    return OperatorTransform(toks[KBU.OPERATOR_S],
                             tuple([toks[KBU.RIGHT_S]]))


def buildTernaryTransformComponent(toks):
    return OperatorTransform(toks[KBU.OPERATOR_S],
                             (toks[KBU.SOURCE_S],
                              toks[KBU.REGEX_S],
                              toks[KBU.REPLACE_S]))


def addRebind(toks):
    if KBU.TARGET_S in toks:
        toks[KBU.TRANSFORM_S][0].set_rebind(toks[KBU.TARGET_S][0])
    return toks[KBU.TRANSFORM_S][0]


# Hotloaded Transform Operators
BINARY_TRANS_OP = pp.Forward()
UNARY_TRANS_OP = pp.Forward()
TERNARY_TRANS_OP = pp.Forward()

rebind = PU.ARROW + VALBIND

# transform: ( bind op val|bind -> bind)
unary_transform_core = PU.N(KBU.OPERATOR_S, UNARY_TRANS_OP) \
    + PU.N(KBU.RIGHT_S, VALBIND)

binary_transform_core = PU.N(KBU.LEFT_S, VALBIND) \
    + PU.N(KBU.OPERATOR_S, BINARY_TRANS_OP) \
    + PU.N(KBU.RIGHT_S, VALBIND)

ternary_transform_core = PU.N(KBU.SOURCE_S, VALBIND) \
    + PU.N(KBU.OPERATOR_S, TERNARY_TRANS_OP) \
    + PU.N(KBU.REGEX_S, VALBIND) \
    + PU.N(KBU.REPLACE_S, VALBIND)

transform_core = PU.NG(KBU.TRANSFORM_S,
                       pp.Or([binary_transform_core,
                              ternary_transform_core,
                              unary_transform_core])) \
                              + PU.op(PU.N(KBU.TARGET_S, rebind))

transforms = transform_core + pp.ZeroOrMore(PU.COMMA + transform_core)

# Actions
binary_transform_core.setParseAction(buildBinaryTransformComponent)
unary_transform_core.setParseAction(buildUnaryTransformComponent)
ternary_transform_core.setParseAction(buildTernaryTransformComponent)

transform_core.setParseAction(addRebind)
transforms.setParseAction(lambda toks: Transform(toks[:]))


# Main Parser:
def parseString(in_string):
    return transforms.parseString(in_string)[0]
