"""
These are proxies for literal symbols to use in printing
They are *only* prepared, to guarantee they exist,
but *not* reified,
as the print system can hold run time overrides if necessary.
The print system itself understands prepared values and will reify as necessary

"""
from __future__ import annotations
import acab
from  acab.core.value.sentence import Sentence

config = acab.config

# Non-printing types
OBVIOUS_TYPES      = config.all_of().print.silent_types(wrapper=set)

print_symbols = {x:pgroup.get(x, x) for pgroup in config.symbols for x in pgroup}
# Symbols
# Not called, because the spec is looked up at print time for overrides
ANON_VALUE_SYM     = config.any_of().symbols.ANON_VALUE()
AT_BIND_SYM        = config.any_of().symbols.AT_BIND()
BIND_SYM           = config.any_of().symbols.BIND()
END_SYM            = config.any_of().symbols.END()
FALLBACK_MODAL_SYM = config.any_of().symbols.FALLBACK_MODAL()
FUNC_SYM           = config.any_of().symbols.FUNC()
NEGATION_SYM       = config.any_of().symbols.NEGATION()
QUERY_SYM          = config.any_of().symbols.QUERY()
REBIND_SYM         = config.any_of().symbols.REBIND()
SUGAR_SYM          = config.any_of().symbols.SUGAR()
TAG_SYM            = config.any_of().symbols.TAG()
TYPE_BASE          = config.any_of().symbols.TYPE_BASE()

# Wrapping
CONTAINER_JOIN_P   = config.any_of().print.patterns.CONTAINER_JOIN()
PARAM_JOIN_P       = config.any_of().print.patterns.PARAM_JOIN()
PRINT_SEPARATOR_P  = config.any_of().print.patterns.PRINT_SEPARATOR()
REGEX_WRAP_P       = config.any_of().print.patterns.REGEX_WRAP()
SEN_JOIN_P         = config.any_of().print.patterns.SEN_JOIN()
STR_WRAP_P         = config.any_of().print.patterns.STR_WRAP()
TAB_P              = config.any_of().print.patterns.TAB()
WRAP_FORMAT_P      = config.any_of().print.patterns.WRAP_FORMAT()
PARAM_WRAP         = config.any_of().print.patterns.PARAM_WRAP()

SPACE              = config.any_of().print.patterns.SPACE()
INDENT             = config.any_of().print.patterns.INDENT()
