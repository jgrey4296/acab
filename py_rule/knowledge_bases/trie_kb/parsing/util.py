"""
Pyparsing based parser to turn strings into [FactNode],
capable of parsing  multiple facts
"""
import pyparsing as pp
from py_rule.knowledge_bases.trie_kb import util as KBU

# Create parsers for Exclusion operators:
# Automatically uses string definitions in KBU
DOT = pp.Keyword(KBU.EXOP_lookup[KBU.EXOP.DOT],
                 identChars=KBU.EXOP_lookup[KBU.EXOP.EX])
EX = pp.Keyword(KBU.EXOP_lookup[KBU.EXOP.EX],
                identChars=KBU.EXOP_lookup[KBU.EXOP.DOT])

DOT.setParseAction(lambda t: KBU.EXOP.DOT)
EX.setParseAction(lambda t: KBU.EXOP.EX)

EL_OPERATOR = pp.Or([EX, DOT])
