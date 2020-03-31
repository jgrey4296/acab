"""
Pyparsing based parser to turn strings into [FactNode],
capable of parsing  multiple facts
"""
import pyparsing as pp
from py_rule.working_memory.trie_wm import util as WMU

# Create parsers for Exclusion operators:
# Automatically uses string definitions in WMU
DOT = pp.Keyword(WMU.EXOP_lookup[WMU.EXOP.DOT],
                 identChars=WMU.EXOP_lookup[WMU.EXOP.EX])
EX = pp.Keyword(WMU.EXOP_lookup[WMU.EXOP.EX],
                identChars=WMU.EXOP_lookup[WMU.EXOP.DOT])

DOT.setParseAction(lambda t: WMU.EXOP.DOT)
EX.setParseAction(lambda t: WMU.EXOP.EX)

EL_OPERATOR = pp.Or([EX, DOT])

EL_OPERATOR.setName("ExclusionOperator")
