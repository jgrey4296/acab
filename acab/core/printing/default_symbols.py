from acab.core.config.config import AcabConfig
from  acab.core.value.sentence import Sentence

config = AcabConfig()


# Non-printing types
OBVIOUS_TYPES      = config.prepare("Print.Data", "SUPPRESSION_TYPES", actions=[AcabConfig.actions_e.SPLIT])

# Symbols
ANON_VALUE_SYM     = config.prepare("Symbols", "ANON_VALUE")
AT_BIND_SYM        = config.prepare("Symbols", "AT_BIND")
BIND_SYM           = config.prepare("Symbols", "BIND")
END_SYM            = config.prepare("Symbols", "END")
FALLBACK_MODAL_SYM = config.prepare("Symbols", "FALLBACK_MODAL", actions=[AcabConfig.actions_e.STRIPQUOTE])
FUNC_SYM           = config.prepare("Symbols", "FUNC")
NEGATION_SYM       = config.prepare("Symbols", "NEGATION")
QUERY_SYM          = config.prepare("Symbols", "QUERY")
REBIND_SYM         = config.prepare("Symbols", "REBIND")
SUGAR_SYM          = config.prepare("Symbols", "SUGAR")
TAG_SYM            = config.prepare("Symbols", "TAG")

# Wrapping
CONTAINER_JOIN_P   = config.prepare("Print.Patterns", "CONTAINER_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE, AcabConfig.actions_e.UNESCAPE])
PARAM_JOIN_P       = config.prepare("Print.Patterns", "PARAM_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])
PRINT_SEPARATOR_P  = config.prepare("Print.Patterns", "PRINT_SEPARATOR", actions=[AcabConfig.actions_e.STRIPQUOTE, AcabConfig.actions_e.UNESCAPE])
REGEX_WRAP_P       = config.prepare("Print.Patterns", "REGEX_WRAP")
SEN_JOIN_P         = config.prepare("Print.Patterns", "SEN_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])
STR_WRAP_P         = config.prepare("Print.Patterns", "STR_WRAP", actions=[AcabConfig.actions_e.UNESCAPE])
TAB_P              = config.prepare("Print.Patterns", "TAB", actions=[AcabConfig.actions_e.STRIPQUOTE])
WRAP_FORMAT_P      = config.prepare("Print.Patterns", "WRAP_FORMAT")
PARAM_WRAP         = config.prepare("Print.Patterns", "PARAM_WRAP")

SPACE              = config.prepare("Print.Patterns", "SPACE", actions=[config.actions_e.STRIPQUOTE])()
INDENT             = config.prepare("Print.Patterns", "INDENT", actions=[config.actions_e.STRIPQUOTE])()
