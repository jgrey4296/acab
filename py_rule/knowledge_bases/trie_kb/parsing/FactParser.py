"""
Pyparsing based parser to turn strings into [FactNode],
capable of parsing  multiple facts
"""
import logging as root_logger
import pyparsing as pp
from py_rule.knowledge_bases.trie_kb.nodes.fact_node import FactNode
from py_rule.knowledge_bases.trie_kb import util as KBU
from py_rule.knowledge_bases.trie_kb.parsing import util as KBPU
from py_rule.abstract.parsing import util as PU

logging = root_logger.getLogger(__name__)
# Hotload insertion points:
TYPE_ANNOTATION = pp.Forward()
OTHER_VALS = pp.Forward()


def make_node(toks):
    """ Make a Factnode, with a parsed value,
    and any additional data
    """
    value = None
    data = {KBU.BIND_S: False,
            KBU.OPERATOR_S: KBU.EXOP.DOT}
    if KBU.BIND_S in toks:
        # The node is a variable
        assert(isinstance(toks[KBU.BIND_S][0], tuple))
        value = toks[KBU.BIND_S][0][1]
        data[KBU.VALUE_TYPE_S] = KBU.NAME_S
        data[KBU.BIND_S] = True
    elif KBU.VALUE_S in toks:
        # The node is a value
        assert(isinstance(toks[KBU.VALUE_S], tuple))
        value = toks[KBU.VALUE_S][1]
        data[KBU.VALUE_TYPE_S] = toks[KBU.VALUE_S][0]
    else:
        raise SyntaxError("Unplanned parse type")
    return FactNode(value, data=data)


def add_annotations(toks):
    """ Add additional data to a node """
    data = {}
    if KBU.OPERATOR_S in toks:
        data[KBU.OPERATOR_S] = toks[KBU.OPERATOR_S][0]
    if KBU.ANNOTATION_S in toks:
        data.update({x: y for x, y in toks[KBU.ANNOTATION_S]})
    toks[KBU.NODE_S]._data.update(data)
    return toks.node


def PARAM_CORE(mid=None, end=None):
    """ Construct a parameterised core parser """
    if mid is None:
        mid = pp.Empty()
    if end is None:
        end = PU.NG(KBU.OPERATOR_S, KBPU.EL_OPERATOR)
    else:
        end = pp.Empty()
    parser = PU.N(KBU.NODE_S, VALBIND) \
        + PU.op(PU.OPAR + PU.NG(KBU.ANNOTATION_S, mid) + PU.CPAR) + end
    parser.setParseAction(add_annotations)
    return parser


# alt for actions, PARAM_CORE
VALBIND = pp.Or([PU.N(KBU.BIND_S, PU.BIND),
                 PU.N(KBU.VALUE_S, pp.Or([PU.BASIC_VALUE, OTHER_VALS]))])
VALBIND.setParseAction(make_node)

# Core = a. | b! | $a. | $b!
PARAM_BINDING_CORE = PARAM_CORE(TYPE_ANNOTATION)
PARAM_BINDING_END = PARAM_CORE(TYPE_ANNOTATION, end=True)

# Basic Sentences without Annotations:
basic_fact_string = pp.ZeroOrMore(PARAM_CORE()) + PARAM_CORE(end=True)

# Sentences with basic sentences as annotations
param_fact_string = pp.ZeroOrMore(PARAM_BINDING_CORE) + PARAM_BINDING_END
param_fact_strings = param_fact_string \
    + pp.ZeroOrMore(PU.COMMA + param_fact_string)

# Actions
param_fact_string.setParseAction(PU.construct_sentence)
basic_fact_string.setParseAction(PU.construct_sentence)


# MAIN PARSER:
def parseString(in_string):
    """ str -> [[Node]] """
    return param_fact_strings.parseString(in_string)[:]
