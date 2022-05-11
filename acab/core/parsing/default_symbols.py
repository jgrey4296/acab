"""
Enforce a default set of symbols to be retrieved
from config files
"""
from acab.core.config.config import AcabConfig
config = AcabConfig()

SymbolDict      = config.prepare("Symbols", _type=dict)()

OPERATOR_SYNTAX = config.prepare("Parse.Patterns", "OPERATOR_SYNTAX")()
WORD_COMPONENT  = config.prepare("Parse.Patterns", "WORD_COMPONENT")()

END              = SymbolDict["END"]
FUNC             = SymbolDict["FUNC"]
BIND             = SymbolDict["BIND"]
AT_BIND          = SymbolDict["AT_BIND"]
QUERY            = SymbolDict["QUERY"]
TAG              = SymbolDict["TAG"]
NEGATION         = SymbolDict["NEGATION"]
TYPE_SEN         = SymbolDict["TYPE_SEN"]
FLATTEN          = SymbolDict['FLATTEN']
SHARP            = SymbolDict['SHARP']

COLLAPSE_CONTEXT = config.attr.Aliases.CTX_COLLAPSE
