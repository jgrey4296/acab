"""
Pyparsing based parser to turn strings into [FactNode],
capable of parsing  multiple facts
"""
import pyparsing as pp
from py_rule.knowledge_bases.trie_kb import util

# Base Defs
DOT = pp.Keyword(util.EXOP_lookup[util.EXOP.DOT], identChars=util.EXOP_lookup[util.EXOP.EX])
EX = pp.Keyword(util.EXOP_lookup[util.EXOP.EX], identChars=util.EXOP_lookup[util.EXOP.DOT])
DOT.setParseAction(lambda t: util.EXOP.DOT)
EX.setParseAction(lambda t: util.EXOP.EX)

EL_OPERATOR = pp.Or([EX, DOT])
