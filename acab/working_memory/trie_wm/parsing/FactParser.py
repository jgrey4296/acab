"""
Pyparsing based parser to turn strings into [FactNode],
capable of parsing  multiple facts
"""
import logging as root_logger
import pyparsing as pp

from acab.config import AcabConfig
from acab.abstract.parsing import util as PU

from acab.abstract.core.value import AcabValue

from acab.working_memory.trie_wm import util as WMU
from acab.working_memory.trie_wm.parsing import util as WMPU

util = AcabConfig.Get()
SEN_S = util("Parsing.Structure", "SEN_S")
BIND_S = util("Parsing.Structure", "BIND_S")
AT_BIND_S = util("Parsing.Structure", "AT_BIND_S")
VALUE_S = util("Parsing.Structure", "VALUE_S")
VALUE_TYPE_S = util("Parsing.Structure", "VALUE_TYPE_S")
OPERATOR_S = util("Parsing.Structure", "OPERATOR_S")
ANNOTATION_S = util("WorkingMemory.TrieWM", "ANNOTATION_S")
NODE_S = util("WorkingMemory.TrieWM", "NODE_S")


logging = root_logger.getLogger(__name__)
# Hotload insertion points:
HOTLOAD_ANNOTATIONS= pp.Forward()
HOTLOAD_VALUES = pp.Forward()


def make_value(toks):
    """ Make a value coupled with additional data """
    value = None
    data = {}
    if BIND_S in toks:
        # is variable
        assert(isinstance(toks[BIND_S][0], tuple))
        value = toks[BIND_S][0][1]
        data[BIND_S] = True
    elif AT_BIND_S in toks:
        # is a reference
        # (can only be at head of a sentence)
        assert(isinstance(toks[AT_BIND_S][0], tuple))
        value = toks[AT_BIND_S][0][1]
        data[BIND_S] = AT_BIND_S
    elif VALUE_S in toks:
        # is an actual value
        assert(isinstance(toks[VALUE_S], tuple))
        value = toks[VALUE_S][1]
        data[VALUE_TYPE_S] = toks[VALUE_S][0]
    else:
        raise SyntaxError("Unplanned parse type")

    return AcabValue.safe_make(value, data=data)


def add_annotations(toks):
    """ Add additional data to a node """
    value_data = WMU.DEFAULT_NODE_DATA.copy()
    if OPERATOR_S in toks:
        value_data[OPERATOR_S] = toks[OPERATOR_S][0]
    if ANNOTATION_S in toks:
        value_data.update({x: y for x, y in toks[ANNOTATION_S]})
    toks[NODE_S]._data.update(value_data)
    return toks.node


def PARAM_CORE(mid=None, end=None):
    """ Construct a parameterised core parser """
    if mid is None:
        mid = pp.Empty()
    if end is None:
        end = PU.NG(OPERATOR_S, WMPU.EL_OPERATOR)
    else:
        end = pp.Empty()
    parser = PU.N(NODE_S, VALBIND) \
        + PU.op(PU.OPAR + PU.NG(ANNOTATION_S, mid) + PU.CPAR) + end
    parser.setParseAction(add_annotations)
    return parser


# alt for actions, PARAM_CORE
VALBIND = pp.Or([PU.N(BIND_S, PU.BIND),
                 PU.N(AT_BIND_S, PU.AT_BIND),
                 PU.N(VALUE_S, pp.Or([PU.BASIC_VALUE, HOTLOAD_VALUES]))])
VALBIND.setParseAction(make_value)

# Core = a. | b! | $a. | $b!
PARAM_BINDING_CORE = PARAM_CORE(HOTLOAD_ANNOTATIONS)
PARAM_BINDING_END = PARAM_CORE(HOTLOAD_ANNOTATIONS, end=True)

# Basic Sentences without Annotations:
BASIC_SEN = PU.op(PU.NEGATION_SYMBOL) + pp.NotAny(PU.END) \
    + PU.NG(SEN_S,  pp.ZeroOrMore(PARAM_CORE()) + PARAM_CORE(end=True))

SEN_STATEMENT = pp.Forward()

# Sentences with basic sentences as annotations
PARAM_SEN = PU.op(PU.NEGATION_SYMBOL) + pp.NotAny(PU.END) \
    + PU.NG(SEN_S, pp.ZeroOrMore(PARAM_BINDING_CORE) + PARAM_BINDING_END)
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
HOTLOAD_ANNOTATIONS.setName("Annotations")
HOTLOAD_VALUES.setName("HotloadValues")
SEN_STATEMENT.setName("SentenceStatement")

parse_point = PARAM_SEN_PLURAL

# MAIN PARSER:
def parseString(in_string):
    """ str -> [[Value]] """
    return parse_point.parseString(in_string)[:]
