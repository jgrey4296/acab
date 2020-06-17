"""
Pyparsing based parser to turn strings into [FactNode],
capable of parsing  multiple facts
"""
import pyparsing as pp
from acab.working_memory.trie_wm import util as WMU
from acab.abstract.printing import util as PrU

# Create parsers for Exclusion operators:
# Automatically uses string definitions in WMU
DOT = pp.Keyword(PrU.MODAL_LOOKUPS[WMU.EXOP.DOT],
                 identChars=PrU.MODAL_LOOKUPS[WMU.EXOP.EX])
EX = pp.Keyword(PrU.MODAL_LOOKUPS[WMU.EXOP.EX],
                identChars=PrU.MODAL_LOOKUPS[WMU.EXOP.DOT])

DOT.setParseAction(lambda t: WMU.EXOP.DOT)
EX.setParseAction(lambda t: WMU.EXOP.EX)

EL_OPERATOR = pp.Or([EX, DOT])

EL_OPERATOR.setName("ExclusionOperator")
