""" Cross-module utilities for the rule engines """
from enum import Enum
from random import choice
import pyparsing as pp

# Globally constant strings:

# Basic constants
NAME_S       = "name"
BIND_S       = "bind"
AT_BIND_S    = "at_bind_"
CONSTRAINT_S = "constraints"
OPERATOR_S   = "operator"
VALUE_S      = "value"
VALUE_TYPE_S = "value_type"
SEN_S        = "sentence"
NEGATION_S   = "negated"
FALLBACK_S   = "fallback"
ARG_S        = "args"
STATEMENT_S  = "statement"
END_S        = "end"

# Core Value Types
FLOAT_S      = "float"
INT_S        = "int"
#Use to signify a decimal, eg: 34d423 = 34.423
DECIMAL_S    = "d"

REGEX_S      = "regex"
STRING_S     = "string"

ACTION_S     = "action"
QUERY_S      = "query"
TRANSFORM_S  = "transform"

# Trie Root Node Name
ROOT_S       = "__root"

# Parser Constants
WORD_COMPONENT_S  = pp.alphas + "_"
OPERATOR_SYNTAX_S = "%^&*_-+={}[]|<>?~§';⊂⊃∨∧⧼⧽¿£ΔΣΩ∩∪√∀∈∃¬∄⟙⟘⊢∴◇□⚬"

# Rule Constants
LAYER_QUERY_RULE_BIND_S = "rule"
RULE_S = "rule"

# Higher Level Structure Heads
RULE_HEAD_S      = "ρ"
QUERY_HEAD_S     = "ι"
TRANSFORM_HEAD_S = "τ"
ACTION_HEAD_S    = "α"

STRUCTURE_S      = "σ"
FUNC_S           = "λ"

