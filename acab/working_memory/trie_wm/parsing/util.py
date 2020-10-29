"""
Pyparsing based parser to turn strings into [FactNode],
capable of parsing  multiple facts
"""
# pylint: disable=bad-whitespace,unnecessary-comprehension
import pyparsing as pp

from acab.config import AcabConfig
from acab.abstract.parsing import util as PU
from acab.abstract.parsing.util import N, NG
from acab.abstract.parsing.consts import OPAR, CPAR
from acab.abstract.parsing import funcs as Pfunc

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence

from acab.abstract.rule.query import Query, QueryComponent
from acab.abstract.rule.action import ActionComponent, Action
from acab.abstract.rule.transform import TransformComponent, Transform
from acab.abstract.rule.production_operator import ProductionContainer
from acab.abstract.rule.rule import Rule

from acab.error.acab_parse_exception import AcabParseException
from acab.working_memory.trie_wm import util as WMU
from acab.working_memory.trie_wm.fact_node import FactNode

util = AcabConfig.Get()

ACTION_S         = util("Parsing.Structure", "ACTION_S")
ANNOTATION_S     = util("WorkingMemory.TrieWM", "ANNOTATION_S")
AT_BIND_S        = util("Parsing.Structure", "AT_BIND_S")
BIND_S           = util("Parsing.Structure", "BIND_S")
CONSTRAINT_S     = util("Parsing.Structure", "CONSTRAINT_S")
DEFAULT_ACTION_S = util("Parsing.Structure", "DEFAULT_ACTION_S")
QUERY_FALLBACK_S  = util("Parsing.Structure", "QUERY_FALLBACK_S")
LEFT_S           = util("WorkingMemory.TrieWM", "LEFT_S")
NEGATION_S       = util("Parsing.Structure", "NEGATION_S")
NODE_S           = util("WorkingMemory.TrieWM", "NODE_S")
OPERATOR_S       = util("Parsing.Structure", "OPERATOR_S")
QUERY_S          = util("Parsing.Structure", "QUERY_S")
RIGHT_S          = util("WorkingMemory.TrieWM", "RIGHT_S")
SEN_S            = util("Parsing.Structure", "SEN_S")
TARGET_S         = util("WorkingMemory.TrieWM", "TARGET_S")
TRANSFORM_S      = util("Parsing.Structure", "TRANSFORM_S")
VALUE_S          = util("Parsing.Structure", "VALUE_S")
VALUE_TYPE_S     = util("Parsing.Structure", "VALUE_TYPE_S")

EXOP_DOT_SYMBOL_S = util("WorkingMemory.TrieWM.Symbols", "EXOP.DOT")
EXOP_EX_SYMBOL_S = util("WorkingMemory.TrieWM.Symbols", "EXOP.EX")

# Create parsers for Exclusion operators:
DOT = pp.Keyword(EXOP_DOT_SYMBOL_S, identChars=EXOP_EX_SYMBOL_S)
EX = pp.Keyword(EXOP_EX_SYMBOL_S, identChars=EXOP_DOT_SYMBOL_S)

DOT.setParseAction(lambda t: WMU.EXOP.DOT)
EX.setParseAction(lambda t: WMU.EXOP.EX)

EL_OPERATOR = pp.Or([EX, DOT])

EL_OPERATOR.setName("ExclusionOperator")

HOTLOAD_VALUES = pp.Forward()
HOTLOAD_VALUES.setName("HotloadValues")


def make_value(toks):
    """ Make a value coupled with additional data """
    value = None
    data = {}
    # TODO: link type primitives with type system
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


VALBIND = pp.Or([N(BIND_S, PU.BIND),
                 N(AT_BIND_S, PU.AT_BIND),
                 N(VALUE_S, pp.Or([PU.BASIC_VALUE, HOTLOAD_VALUES]))])
VALBIND.setParseAction(make_value)
VALBIND.setName("ValBind")

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
        end = Pfunc.NG(OPERATOR_S, EL_OPERATOR)
    else:
        end = pp.Empty()
    # TODO fix this
    parser = N(NODE_S, VALBIND) \
        + PU.op(OPAR + NG(ANNOTATION_S, mid) + CPAR) + end
    parser.setParseAction(add_annotations)
    return parser


def build_constraint_list(toks):
    """ Build a constraint list """
    return (CONSTRAINT_S, [x[1] for x in toks[:]])

def build_query_component(toks):
    """ Build a comparison """
    op = toks[OPERATOR_S][0]
    return (CONSTRAINT_S, QueryComponent(op, param=toks[VALUE_S]))

def build_clause(toks):
    # detect negation and annotate the clause with it
    data = { QUERY_S : True,
             NEGATION_S : False,
             QUERY_FALLBACK_S : None }
    if QUERY_FALLBACK_S in toks:
        if NEGATION_S in toks:
            raise AcabParseException("Negated Fallback clauses don't make sense")
        data[QUERY_FALLBACK_S] = toks[QUERY_FALLBACK_S][:]
    if NEGATION_S in toks:
        data[NEGATION_S] = True

    return toks[0].set_data(data)

def build_query(toks):
    query = Query(toks[:])
    return (query.type, query)

def build_assignment(toks):
    return (toks[0][1], toks[1])

def build_action_component(toks):
    params = []
    if LEFT_S in toks:
        params.append(toks[LEFT_S])
    if RIGHT_S in toks:
        params = toks[RIGHT_S][:]
    op = toks[OPERATOR_S][0]
    filtered_params = [x[0] if len(x) == 1 else x for x in params]
    return ActionComponent(op, filtered_params, sugared=LEFT_S in toks)

def build_action(toks):
    clauses = [x if isinstance(x, ActionComponent)
               else ActionComponent(Sentence.build([DEFAULT_ACTION_S]), [x]) for x in toks]
    act = Action(clauses)

    return (act.type, act)

def build_transform_component(toks):
    params = []
    if LEFT_S in toks:
        params.append(toks[LEFT_S][0])
    params += toks[RIGHT_S][:]

    op = toks[OPERATOR_S][0]
    if isinstance(op, str):
        op = Sentence.build([op])

    rebind = toks[TARGET_S][0]
    filtered_params = [x[0] if len(x) == 1 else x for x in params]

    return TransformComponent(op, filtered_params, rebind=rebind, sugared=LEFT_S in toks)

def build_transform(toks):
    trans = Transform(toks[:])
    return (trans.type, trans)

def build_rule(toks):

    # Get Conditions
    if QUERY_S in toks:
        c = toks[QUERY_S][0][1]
        assert(isinstance(c, ProductionContainer))
    else:
        c = None

    # Get Transform
    if TRANSFORM_S in toks:
        t = toks[TRANSFORM_S][0][1]
        assert(isinstance(t, ProductionContainer))
    else:
        t = None

    # Get Action
    if ACTION_S in toks:
        a = toks[ACTION_S][0][1]
        assert(isinstance(a, ProductionContainer))
    else:
        a = None


    rule = Rule(c, action=a, transform=t)
    return (rule.type, rule)
