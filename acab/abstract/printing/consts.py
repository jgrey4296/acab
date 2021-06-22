#!/usr/bin/env python3

from acab.abstract.config.config import AcabConfig
from  acab.abstract.core.values import Sentence

config = AcabConfig.Get()

# Primitive Types
STRING_SEN              = Sentence.build([config.value("Type.Primitive", "STRING")])
REGEX_SEN               = Sentence.build([config.value("Type.Primitive", "REGEX")])
SEN_SEN                 = Sentence.build([config.value("Type.Primitive", "SENTENCE")])

# Data Access
TYPE_INSTANCE_V         = config.value("Value.Structure", "TYPE_INSTANCE")
QUERY_V                 = config.value("Value.Structure", "QUERY")
BIND_V                  = config.value("Value.Structure", "BIND")
CONSTRAINT_V            = config.value("Value.Structure", "CONSTRAINT")
AT_BIND_V               = config.value("Value.Structure", "AT_BIND")
NEGATION_V              = config.value("Value.Structure", "NEGATION")
OPERATOR_V              = config.value("Value.Structure", "OPERATOR")
TAG_V                   = config.value("Value.Structure", "TAG")

# Non-printing types
OBVIOUS_TYPES           = config.prepare("Print.Data", "SUPPRESSION_TYPES", actions=[AcabConfig.actions_e.SPLIT])

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
CONTAINER_JOIN_P        = config.prepare("Print.Patterns", "CONTAINER_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])
PARAM_JOIN_P            = config.prepare("Print.Patterns", "PARAM_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])
PRINT_SENTINEL_JOIN_P   = config.prepare("Print.Patterns", "PRINT_SENTINEL_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])
REGEX_WRAP_P            = config.prepare("Print.Patterns", "REGEX_WRAP")
SEN_JOIN_P              = config.prepare("Print.Patterns", "SEN_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])
STR_WRAP_P              = config.prepare("Print.Patterns", "STR_WRAP")
TAB_P                   = config.prepare("Print.Patterns", "TAB", actions=[AcabConfig.actions_e.STRIPQUOTE])
WRAP_FORMAT_P           = config.prepare("Print.Patterns", "WRAP_FORMAT")
