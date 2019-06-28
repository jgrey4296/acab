"""
Pyparsing based parser to turn strings into [FactNode],
capable of parsing  multiple facts
"""
import logging as root_logger
import pyparsing as pp
import IPython
from py_rule.utils import EXOP
from py_rule.trie.typing.ex_types import MonoTypeVar
from py_rule.trie.nodes.fact_node import FactNode
from py_rule.trie.nodes.trie_node import TrieNode
from py_rule.abstract.sentence import Sentence

#UTILITIES
logging = root_logger.getLogger(__name__)
pp.ParserElement.setDefaultWhitespaceChars(' \t\r')
s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.lineEnd))
comment = s(pp.dblSlashComment)

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

def construct_fact_string(toks):
    return Sentence(toks[:])

def make_type_dec(toks):
    path = toks.SEN[:]
    baseName = path[-1]
    args = []
    if 'ARGS' in toks:
        args = toks.ARGS[:]
    elif "_type" in baseName._data and baseName._data["_type"] is not None:
        args.append(baseName._type)
        del baseName._data["_type"]
    return ("typedec", MonoTypeVar(baseName, path, args))

def make_node(toks):
    value = None
    data = {'bind' : False,
            'op' : EXOP.DOT}
    if 'bind' in toks:
        value = toks.bind[0][1]
        data['value_type'] = 'name'
        data['bind'] = True
    elif 'value' in toks:
        value = toks.value[1]
        data['value_type'] = toks.value[0]
    return TrieNode(value, data)

def add_annotations(toks):
    data = {}
    if 'op' in toks:
        data['op'] = toks.op[0]
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
NAME.setParseAction(lambda t: ("name", t[0]))
NUM = pp.Word(pp.nums + '-d')
NUM.setParseAction(construct_num)

STRING = pp.dblQuotedString
STRING.setParseAction(lambda t: ("string", t[0].replace('"', '')))

VALUE = pp.Or([NAME, NUM, STRING])

#alt for actions, PARAM_CORE
BIND = DOLLAR + VALUE
VALBIND = pp.Or([N("bind", BIND), N("value", VALUE)])
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
param_fact_string.setParseAction(construct_fact_string)
basic_fact_string.setParseAction(construct_fact_string)

# MAIN PARSER:
def parseString(in_string):
    """ str -> [[Node]] """
    # IPython.embed(simple_prompt=True)
    parsed = param_fact_strings.parseString(in_string)[:]
    return parsed
