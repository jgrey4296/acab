from acab.abstract.config.config import AcabConfig
config = AcabConfig.Get()

OPERATOR_SYNTAX  = config.prepare("Parse.Patterns", "OPERATOR_SYNTAX")()
WORD_COMPONENT   = config.prepare("Parse.Patterns", "WORD_COMPONENT")()

END              = config.prepare("Symbols", "END")()

RULE_HEAD        = config.prepare("Aliases", "RULE")()
QUERY_HEAD       = config.prepare("Aliases", "QUERY")()
TRANSFORM_HEAD   = config.prepare("Aliases", "TRANSFORM")()
ACTION_HEAD      = config.prepare("Aliases", "ACTION")()
FACT_HEAD        = config.prepare("Aliases", "FACT")()
AGENDA_HEAD      = config.prepare("Aliases", "AGENDA")()
LAYER_HEAD       = config.prepare("Aliases", "LAYER")()
PIPE_HEAD        = config.prepare("Aliases", "PIPE")()
COLLAPSE_CONTEXT = config.prepare("Aliases", "CTX_COLLAPSE")()

FUNC             = config.prepare("Symbols", "FUNC")()

BIND             = config.prepare("Symbols", "BIND")()
AT_BIND          = config.prepare("Symbols", "AT_BIND")()

QUERY            = config.prepare("Symbols", "QUERY")()
TAG              = config.prepare("Symbols", "TAG")()

NEGATION         = config.prepare("Symbols", "NEGATION")()
