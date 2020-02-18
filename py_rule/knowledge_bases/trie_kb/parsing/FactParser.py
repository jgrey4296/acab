"""
Pyparsing based parser to turn strings into [FactNode],
capable of parsing  multiple facts
"""
import logging as root_logger
import pyparsing as pp
from py_rule.abstract.sentence import Sentence
from py_rule.abstract.trie.nodes.trie_node import TrieNode
from py_rule.knowledge_bases.trie_kb.nodes.fact_node import FactNode
from py_rule.typing.type_instance import TypeInstance
from py_rule.utils import EXOP, TYPE_DEC_S, BIND_S, OPERATOR_S, VALUE_TYPE_S, VALUE_S, NAME_S, STRING_S, REGEX_S

#UTILITIES
logging = root_logger.getLogger(__name__)
pp.ParserElement.setDefaultWhitespaceChars(' \t\r')
s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.lineEnd))

def NG(name, grp):
    """ Name and Group """
    return pp.Group(grp).setResultsName(name)

def N(name, parser):
    return parser.setResultsName(name)

def construct_num(toks):
    #todo: add in fractions and underscores
    if 'd' in toks[0]:
        return ("float", float(toks[0].replace('d', '.')))
    else:
        return ("int", int(toks[0]))

def construct_sentence(toks):
    return Sentence(toks[:])

def make_type_dec(toks):
    path = toks.SEN[:]
    baseName = path[-1]
    args = []
    if 'ARGS' in toks:
        args = toks.ARGS[:]
    elif TYPE_DEC_S in baseName._data and baseName._data[TYPE_DEC_S] is not None:
        args.append(baseName._type)
        del baseName._data[TYPE_DEC_S]
    return (TYPE_DEC_S, MonoTypeVar(baseName, path, args))

def make_node(toks):
    value = None
    data = {BIND_S : False,
            OPERATOR_S : EXOP.DOT}
    if BIND_S in toks:
        assert(isinstance(toks.bind[0], tuple))
        value = toks.bind[0][1]
        data[VALUE_TYPE_S] = NAME_S
        data[BIND_S] = True
    elif VALUE_S in toks:
        assert(isinstance(toks.value, tuple))
        value = toks.value[1]
        data[VALUE_TYPE_S] = toks.value[0]
    return TrieNode(value, data)

def add_annotations(toks):
    data = {}
    if 'op' in toks:
        data[OPERATOR_S] = toks.op[0]
    if 'annotations' in toks:
        data.update({x:y for x,y in toks.annotations})
    toks.node._data.update(data)
    return toks.node


#Base Defs
DOT = pp.Keyword('.', identChars='!')
EX = pp.Keyword('!', identChars='.')
DOT.setParseAction(lambda t: EXOP.DOT)
EX.setParseAction(lambda t: EXOP.EX)

OPERATOR = pp.Or([EX, DOT])
COMMA = s(pp.Or([pp.Literal(',') + opLn, pp.lineEnd]))
COLON = s(pp.Literal(':'))
DBLCOLON = s(pp.Literal("::"))
end = s(pp.Literal('end'))
sLn = s(pp.White(ws='\n', exact=1))
DOLLAR = s(pp.Literal('$'))
OPAR = s(pp.Literal('('))
CPAR = s(pp.Literal(')'))

basic_fact_string = pp.Forward()
TYPEDEC_CORE = pp.Forward()

NAME = pp.Word(pp.alphas + "_")
NAME.setParseAction(lambda t: (NAME_S, t[0]))
NUM = pp.Word(pp.nums + '-d')
NUM.setParseAction(construct_num)

STRING = pp.dblQuotedString
STRING.setParseAction(lambda t: (STRING_S, t[0].replace('"', '')))

REGEX = pp.Regex(r'/.+?/')
REGEX.setParseAction(lambda t: (REGEX_S, t[0][1:-1]))

OTHER_VALS = pp.Forward()

VALUE = pp.Or([NAME, NUM, STRING, REGEX])

#alt for actions, PARAM_CORE
BIND = DOLLAR + VALUE
VALBIND = pp.Or([N(BIND_S, BIND), N(VALUE_S, pp.Or([VALUE, OTHER_VALS]))])
VALBIND.setParseAction(make_node)

TYPEDEC_CORE << DBLCOLON + N("SEN", basic_fact_string) \
    + N("ARGS", op(OPAR + pp.delimitedList(TYPEDEC_CORE, ', ', combine=False) + CPAR))
TYPEDEC_CORE.setParseAction(lambda t: make_type_dec(t))

def PARAM_CORE(mid=None, end=None):
    if mid is None:
        mid = pp.Empty()
    if end is None:
        end = NG("op", OPERATOR)
    else:
        end = pp.Empty()
    parser = N("node", VALBIND) + op(OPAR + NG("annotations", mid) + CPAR) + end
    parser.setParseAction(add_annotations)
    return parser

#Core = a. | b! | $a. | $b!
PARAM_BINDING_CORE = PARAM_CORE(TYPEDEC_CORE)
PARAM_BINDING_END = PARAM_CORE(TYPEDEC_CORE, end=True)

#todo: make these into sentences
basic_fact_string << pp.ZeroOrMore(PARAM_CORE()) + PARAM_CORE(end=True)
param_fact_string = pp.ZeroOrMore(PARAM_BINDING_CORE) + PARAM_BINDING_END
param_fact_strings = param_fact_string + pp.ZeroOrMore(COMMA + param_fact_string)

#Actions
param_fact_string.setParseAction(construct_sentence)
basic_fact_string.setParseAction(construct_sentence)

# MAIN PARSER:
def parseString(in_string):
    """ str -> [[Node]] """
    return param_fact_strings.parseString(in_string)[:]
