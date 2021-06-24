from acab.abstract.config.config import AcabConfig
config = AcabConfig.Get()

OPERATOR_SYNTAX = config.value("Parse.Patterns", "OPERATOR_SYNTAX")
WORD_COMPONENT  = config.value("Parse.Patterns", "WORD_COMPONENT")

END              = config.value("Symbols", "END", actions=[AcabConfig.actions_e.LITERAL])

RULE_HEAD        = config.value("Aliases", "RULE", actions=[AcabConfig.actions_e.KEYWORD])
QUERY_HEAD       = config.value("Aliases", "QUERY", actions=[AcabConfig.actions_e.KEYWORD])
TRANSFORM_HEAD   = config.value("Aliases", "TRANSFORM", actions=[AcabConfig.actions_e.KEYWORD])
ACTION_HEAD      = config.value("Aliases", "ACTION", actions=[AcabConfig.actions_e.KEYWORD])
FACT_HEAD        = config.value("Aliases", "FACT", actions=[AcabConfig.actions_e.KEYWORD])
COLLAPSE_CONTEXT = config.value("Aliases", "CTX_COLLAPSE", actions=[AcabConfig.actions_e.LITERAL])
AGENDA_HEAD      = config.value("Aliases", "AGENDA", actions=[AcabConfig.actions_e.KEYWORD])
LAYER_HEAD       = config.value("Aliases", "LAYER", actions=[AcabConfig.actions_e.KEYWORD])
PIPE_HEAD        = config.value("Aliases", "PIPE", actions=[AcabConfig.actions_e.KEYWORD])

FUNC      = config.value("Symbols", "FUNC")

BIND      = config.value("Symbols", "BIND", actions=[AcabConfig.actions_e.LITERAL])
AT_BIND   = config.value("Symbols", "AT_BIND", actions=[AcabConfig.actions_e.LITERAL])

QUERY     = config.value("Symbols", "QUERY", actions=[AcabConfig.actions_e.LITERAL])
TAG       = config.value("Symbols", "TAG", actions=[AcabConfig.actions_e.LITERAL])

NEGATION  = config.value("Symbols", "NEGATION", actions=[AcabConfig.actions_e.LITERAL])
