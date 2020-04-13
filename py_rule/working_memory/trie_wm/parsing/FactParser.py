"""
Pyparsing based parser to turn strings into [FactNode],
capable of parsing  multiple facts
"""
import logging as root_logger
import pyparsing as pp
from py_rule.working_memory.trie_wm.nodes.fact_node import FactNode
from py_rule.working_memory.trie_wm import util as WMU
from py_rule.working_memory.trie_wm.parsing import util as WMPU
from py_rule.abstract.parsing import util as PU
from py_rule import util

logging = root_logger.getLogger(__name__)
# Hotload insertion points:
HOTLOAD_ANNOTATIONS= pp.Forward()
HOTLOAD_VALUES = pp.Forward()


def make_node(toks):
    """ Make a Factnode, with a parsed value,
    and any additional data
    """
    value = None
    data = WMU.DEFAULT_NODE_DATA.copy()
    if WMU.BIND_S in toks:
        # The node is a variable
        assert(isinstance(toks[WMU.BIND_S][0], tuple))
        value = toks[WMU.BIND_S][0][1]
        data[WMU.VALUE_TYPE_S] = WMU.NAME_S
        data[WMU.BIND_S] = True
    elif WMU.AT_BIND_S in toks:
        # Node is a reference variable
        # (can only be at head of a sentence)
        assert(isinstance(toks[WMU.AT_BIND_S][0], tuple))
        value = toks[WMU.AT_BIND_S][0][1]
        data[WMU.VALUE_TYPE_S] = WMU.NAME_S
        data[WMU.BIND_S] = True
        data[WMU.AT_BIND_S] = True
    elif WMU.VALUE_S in toks:
        # The node is a value
        assert(isinstance(toks[WMU.VALUE_S], tuple))
        value = toks[WMU.VALUE_S][1]
        data[WMU.VALUE_TYPE_S] = toks[WMU.VALUE_S][0]
    else:
        raise SyntaxError("Unplanned parse type")
    return FactNode(value, data=data)


def add_annotations(toks):
    """ Add additional data to a node """
    data = {}
    if WMU.OPERATOR_S in toks:
        data[WMU.OPERATOR_S] = toks[WMU.OPERATOR_S][0]
    if WMU.ANNOTATION_S in toks:
        data.update({x: y for x, y in toks[WMU.ANNOTATION_S]})
    toks[WMU.NODE_S]._data.update(data)
    return toks.node


def PARAM_CORE(mid=None, end=None):
    """ Construct a parameterised core parser """
    if mid is None:
        mid = pp.Empty()
    if end is None:
        end = PU.NG(WMU.OPERATOR_S, WMPU.EL_OPERATOR)
    else:
        end = pp.Empty()
    parser = PU.N(WMU.NODE_S, VALBIND) \
        + PU.op(PU.OPAR + PU.NG(WMU.ANNOTATION_S, mid) + PU.CPAR) + end
    parser.setParseAction(add_annotations)
    return parser


# alt for actions, PARAM_CORE
VALBIND = pp.Or([PU.N(WMU.BIND_S, PU.BIND),
                 PU.N(WMU.AT_BIND_S, PU.AT_BIND),
                 PU.N(WMU.VALUE_S, pp.Or([PU.BASIC_VALUE, HOTLOAD_VALUES]))])
VALBIND.setParseAction(make_node)

# Core = a. | b! | $a. | $b!
PARAM_BINDING_CORE = PARAM_CORE(HOTLOAD_ANNOTATIONS)
PARAM_BINDING_END = PARAM_CORE(HOTLOAD_ANNOTATIONS, end=True)

# Basic Sentences without Annotations:
BASIC_SEN = PU.op(PU.NEGATION_SYMBOL) + pp.NotAny(PU.END) \
    + PU.NG(util.SEN_S,  pp.ZeroOrMore(PARAM_CORE()) + PARAM_CORE(end=True))

SEN_STATEMENT = pp.Forward()

# Sentences with basic sentences as annotations
PARAM_SEN = PU.op(PU.NEGATION_SYMBOL) + pp.NotAny(PU.END) \
    + PU.NG(util.SEN_S, pp.ZeroOrMore(PARAM_BINDING_CORE) + PARAM_BINDING_END)
PARAM_SEN_PLURAL = pp.delimitedList(PARAM_SEN, delim=PU.DELIM)

SEN_STATEMENT_BODY = pp.OneOrMore(pp.Or([SEN_STATEMENT, PARAM_SEN_PLURAL]) + PU.opLn)
# Statement to specify multiple sub sentences
SEN_STATEMENT << PU.STATEMENT_CONSTRUCTOR(PU.FACT_HEAD,
                                          PARAM_SEN,
                                          SEN_STATEMENT_BODY,
                                          parse_fn=PU.construct_multi_sentences)

# Actions
PARAM_SEN.setParseAction(PU.construct_sentence)
BASIC_SEN.setParseAction(PU.construct_sentence)

# Naming
VALBIND.setName("ValBind")
PARAM_BINDING_CORE.setName("ParamBindCore")
PARAM_BINDING_END.setName("ParamBindEnd")
BASIC_SEN.setName("BasicSentence")
PARAM_SEN.setName("ParameterisedSentence")
PARAM_SEN_PLURAL.setName("ParamSentencePlural")




# parse_point = PARAM_SEN_PLURAL.ignore(PU.COMMENT)
parse_point = PARAM_SEN_PLURAL

# MAIN PARSER:
def parseString(in_string):
    """ str -> [[Node]] """
    return parse_point.parseString(in_string)[:]
