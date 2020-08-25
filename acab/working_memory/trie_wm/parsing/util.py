"""
Pyparsing based parser to turn strings into [FactNode],
capable of parsing  multiple facts
"""
import pyparsing as pp
from acab.working_memory.trie_wm import util as WMU
from acab.abstract.printing import util as PrU
from acab.config import AcabConfig
util = AcabConfig.Get()

EXOP_DOT_S = util("WorkingMemory.TrieWM.Parsing", "EXOP.DOT_S")
EXOP_EX_S = util("WorkingMemory.TrieWM.Parsing", "EXOP.EX_S")

# Create parsers for Exclusion operators:
DOT = pp.Keyword(EXOP_DOT_S, identChars=EXOP_EX_S)
EX = pp.Keyword(EXOP_EX_S, identChars=EXOP_DOT_S)

DOT.setParseAction(lambda t: WMU.EXOP.DOT)
EX.setParseAction(lambda t: WMU.EXOP.EX)

EL_OPERATOR = pp.Or([EX, DOT])

EL_OPERATOR.setName("ExclusionOperator")
