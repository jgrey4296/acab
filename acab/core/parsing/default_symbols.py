"""
Enforce a default set of symbols to be retrieved
from config files
"""
from acab.core.config.config import AcabConfig
config = AcabConfig()

SymbolDict      = config.prepare("Symbols", as_dict=True)()
AliasDict       = config.prepare("Aliases", as_dict=True)()

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

RULE_HEAD        = AliasDict["RULE"]
QUERY_HEAD       = AliasDict["QUERY"]
TRANSFORM_HEAD   = AliasDict["TRANSFORM"]
ACTION_HEAD      = AliasDict["ACTION"]
FACT_HEAD        = AliasDict["FACT"]
AGENDA_HEAD      = AliasDict["AGENDA"]
LAYER_HEAD       = AliasDict["LAYER"]
PIPE_HEAD        = AliasDict["PIPE"]
COLLAPSE_CONTEXT = AliasDict["CTX_COLLAPSE"]
