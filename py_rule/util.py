""" Cross-module utilities for the rule engines """
from enum import Enum
from random import choice
import pyparsing as pp

# Globally constant strings:

# Basic constants
ARG_S        = "args"
AT_BIND_S    = "at_bind_"
BIND_S       = "bind"
CONSTRAINT_S = "constraints"
END_S        = "end"
FALLBACK_S   = "fallback"
NAME_S       = "name"
NEGATION_S   = "negated"
OPERATOR_S   = "operator"
SEN_S        = "sentence"
STATEMENT_S  = "statement"
TAG_S        = "tag"
VALUE_S      = "value"
VALUE_TYPE_S = "value_type"
TYPE_DEC_S    = "type_declaration"
OP_CLASS_S   = "operator_class"

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
RULE_S = "rule"

# Higher Level Structure Heads
RULE_HEAD_S       = "ρ"
QUERY_HEAD_S      = "γ"
TRANSFORM_HEAD_S  = "τ"
ACTION_HEAD_S     = "α"
FACT_HEAD_S       = "Σ"
SUM_HEAD_S        = "Σσ"
STRUCTURE_S       = "σ"
FUNC_S            = "λ"
TYPE_CLASS_S      = "γ"
UUID_HEAD         = "υ"

VAR_SYMBOL_S      = "$"
AT_VAR_SYMBOL_S   = "@"
TAG_SYMBOL_S      = "#"
NEGATION_SYMBOL_S = "~"
QUERY_SYMBOL_S    = "?"

# Default Data for any value:
DEFAULT_VALUE_DATA = {
    BIND_S       : False,
    VALUE_TYPE_S : NAME_S
    }


def rebind_across_contexts(names, values, base):
    assert(isinstance(base, dict))
    assert(isinstance(names, list))
    assert(isinstance(values, list))
    new_base = {}
    new_base.update(base)
    for x,y in zip(names, values):
        new_base[x] = y

    return new_base

