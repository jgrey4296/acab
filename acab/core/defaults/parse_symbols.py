"""
These are actual symbols to be parsed for.

ie: pp.Literal(END)

"""
from acab
config = acab.config

OPERATOR_SYNTAX = "".join(config.all_of().parse.patterns.OPERATOR_SYNTAX())
WORD_COMPONENT  = "".join(config.all_of().parse.patterns.WORD_COMPONENT())

END        = config.any_of().symbols.END()
FUNC       = config.any_of().symbols.FUNC()
BIND       = config.any_of().symbols.BIND()
AT_BIND    = config.any_of().symbols.AT_BIND()
QUERY      = config.any_of().symbols.QUERY()
TAG        = config.any_of().symbols.TAG()
NEGATION   = config.any_of().symbols.NEGATION()
TYPE_SEN   = config.any_of().symbols.TYPE_SEN()
FLATTEN    = config.any_of().symbols.FLATTEN()
SHARP      = config.any_of().symbols.SHARP()

COLLAPSE_CONTEXT = config.any_of().aliases.CTX_COLLAPSE()
