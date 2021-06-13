#!/usr/bin/env python3

from acab.abstract.config.config import AcabConfig
from  acab.abstract.core.values import Sentence

config = AcabConfig.Get()

#Static
ANON_VALUE_P            = config.prepare("Symbols", "ANON_VALUE")
AT_BIND_P               = config.prepare("Symbols", "AT_BIND")
AT_BIND_SYMBOL_V        = config.value("Symbols", "AT_BIND")
BIND_P                  = config.prepare("Symbols", "BIND")
BIND_SYMBOL_V           = config.value("Symbols", "BIND")
END_P                   = config.prepare("Symbols", "END")
END_SYMBOL_V            = config.value("Symbols", "END")
FALLBACK_MODAL_P        = config.prepare("Symbols", "FALLBACK_MODAL", actions=[AcabConfig.actions_e.STRIPQUOTE])
FALLBACK_MODAL_SYMBOL_V = config.value("Symbols", "FALLBACK_MODAL", actions=[AcabConfig.actions_e.STRIPQUOTE])
FUNC_P                  = config.prepare("Symbols", "FUNC")
FUNC_SYMBOL_V           = config.value("Symbols", "FUNC")
FUNC_V                  = config.value("Parse.Structure", "FUNC")
NEGATION_P              = config.prepare("Symbols", "NEGATION")
NEGATION_SYMBOL_V       = config.value("Symbols", "NEGATION")
QUERY_SYMBOL_P          = config.prepare("Symbols", "QUERY")
QUERY_SYMBOL_V          = config.value("Symbols", "QUERY")

# Simple Symbols
REBIND_P                = config.prepare("Symbols", "REBIND")
SUGAR_P                 = config.prepare("Symbols", "SUGAR")
TAB_P                   = config.prepare("Print.Patterns", "TAB", actions=[AcabConfig.actions_e.STRIPQUOTE])
TAB_V                   = config.value("Print.Patterns", "TAB", actions=[AcabConfig.actions_e.STRIPQUOTE])
TAG_P                   = config.prepare("Symbols", "TAG")
TAG_SYMBOL_V            = config.value("Symbols", "TAG")

# Primitive Types
STRING_SEN              = Sentence.build([config.value("Type.Primitive", "STRING")])
REGEX_SEN               = Sentence.build([config.value("Type.Primitive", "REGEX")])
SEN_SEN                 = Sentence.build([config.value("Type.Primitive", "SENTENCE")])

# Non-printing types
OBVIOUS_TYPES           = config.prepare("Print.Data", "SUPPRESSION_TYPES", actions=[AcabConfig.actions_e.SPLIT])
OBVIOUS_TYPES_P         = config.prepare("Print.Data", "SUPPRESSION_TYPES", actions=[AcabConfig.actions_e.SPLIT])

# Data Access
TYPE_INSTANCE_V         = config.value("Value.Structure", "TYPE_INSTANCE")
QUERY_V                 = config.value("Value.Structure", "QUERY")
BIND_V                  = config.value("Value.Structure", "BIND")
CONSTRAINT_V            = config.value("Value.Structure", "CONSTRAINT")
AT_BIND_V               = config.value("Value.Structure", "AT_BIND")
NEGATION_V              = config.value("Value.Structure", "NEGATION")
OPERATOR_V              = config.value("Value.Structure", "OPERATOR")
TAG_V                   = config.value("Value.Structure", "TAG")

# Wrapping
REGEX_WRAP_P            = config.prepare("Print.Patterns", "REGEX_WRAP")
STR_WRAP_P              = config.prepare("Print.Patterns", "STR_WRAP")
WRAP_FORMAT_P           = config.prepare("Print.Patterns", "WRAP_FORMAT")
WRAP_FORMAT_V           = config.value("Print.Patterns", "WRAP_FORMAT")
PARAM_JOIN_P            = config.prepare("Print.Patterns", "PARAM_JOIN")
PARAM_JOIN_V            = config.value("Print.Patterns", "PARAM_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])
PRINT_SENTINEL_JOIN_P   = config.prepare("Print.Patterns", "PRINT_SENTINEL_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])
SEN_JOIN_P              = config.prepare("Print.Patterns", "SEN_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])
SEN_JOIN_V              = config.value("Print.Patterns", "SEN_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])
CONTAINER_JOIN_P        = config.prepare("Print.Patterns", "CONTAINER_JOIN")
CONTAINER_JOIN_V        = config.value("Print.Patterns", "CONTAINER_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])
END_V                   = config.value("Parse.Structure", "END")
