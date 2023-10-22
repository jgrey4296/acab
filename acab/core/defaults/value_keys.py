"""
These are enums and values used for AcabValue internal data attributes.

ie: AcabValue().data[BIND] = True

also also includes the modality enum, core type primitives,
and production structure keys

"""
##-- import
from __future__ import annotations

from enum import Enum
from typing import Any

import acab
##-- end import

config = acab.config

DATA_STRUCT_E : Enum = config.all_of().values.structure(wrapper=lambda x: Enum("DATA STRUCT", x))
MODALITIES_E  : Enum = config.all_of().modal.enum_values(wrapper=lambda x: Enum("MODALS", x))

ARG             = DATA_STRUCT_E.ARG
AT_BIND         = DATA_STRUCT_E.AT_BIND
BIND            = DATA_STRUCT_E.BIND
CONSTRAINT      = DATA_STRUCT_E.CONSTRAINT
NEGATION        = DATA_STRUCT_E.NEGATION
PARAMS          = DATA_STRUCT_E.PARAMS
QUERY           = DATA_STRUCT_E.QUERY
QUERY_FALLBACK  = DATA_STRUCT_E.QUERY_FALLBACK
SEMANTIC_HINT   = DATA_STRUCT_E.SEMANTIC_HINT
SEN             = DATA_STRUCT_E.SEN
TAG             = DATA_STRUCT_E.TAG
TYPE_INSTANCE   = DATA_STRUCT_E.TYPE_INSTANCE
FLATTEN         = DATA_STRUCT_E.FLATTEN

# Core elements of value data
# TODO move this out of Value.Structure
OPERATOR       : str = "OPERATOR" # config.any_of().values.structure.OPERATOR()

# TODO refactor to use this instead
STRUCT_COMP_E : Enum = config.all_of().types.structure(wrapper=lambda x: Enum("Struct Comp", x))
# Core Components
QUERY_COMPONENT     : str = "QUERY"     # config.any_of().types.structure.QUERY()
TRANSFORM_COMPONENT : str = "TRANSFORM" # config.any_of().types.structure.TRANSFORM()
ACTION_COMPONENT    : str = "ACTION"    # config.any_of().types.structure.ACTION()

# Core data defaults

TYPE_BASE        = config.data.TYPE_BASE

# TODO refactor to use this instead
TYPE_PRIM_E   : Enum = config.all_of().types.primitive(wrapper=lambda x: Enum("Type Primitives", x))
# Core type primitives
COMPONENT_PRIM =  "COMPONENT"     # config.any_of().type.primitive.COMPONENT()
CONTAINER_PRIM =  "CONTAINER"     # config.any_of().type.primitive.CONTAINER()
OPERATOR_PRIM  =  "OPERATOR_PRIM" # config.any_of().type.primitive.OPERATOR_PRIM()
SENTENCE_PRIM  =  "SENTENCE"      # config.any_of().type.primitive.SENTENCE()
STRING_PRIM    =  "STRING"        # config.any_of().type.primitive.STRING()
REGEX_PRIM     =  "REGEX"         # config.any_of().type.primitive.REGEX()
STRUCT_PRIM    =  "STRUCTURE"     # config.any_of().type.primitive.STRUCTURE()
INSTR_PRIM     =  "INSTRUCT"      # config.any_of().type.primitive.INSTRUCT()
