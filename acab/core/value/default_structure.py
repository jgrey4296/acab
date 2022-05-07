"""
Default elements found in values, loaded from config
"""
from acab import AcabConfig
from enum import Enum

config = AcabConfig()

# TODO refactor to use these instead
DATA_STRUCT_E : Enum = config.prepare("Value.Structure"      , as_enum=True)()
STRUCT_COMP_E : Enum = config.prepare("Structure.Components" , as_enum=True)()
TYPE_PRIM_E   : Enum = config.prepare("Type.Primitive"       , as_enum=True)()
MODALITIES_E  : Enum = config.prepare("MODAL"                , as_enum=True)()

# Core elements of value data
AT_BIND        : str = config.attr.Value.Structure.AT_BIND
BIND           : str = config.attr.Value.Structure.BIND
CONSTRAINT     : str = config.attr.Value.Structure.CONSTRAINT
NEGATION       : str = config.attr.Value.Structure.NEGATION
OPERATOR       : str = config.attr.Value.Structure.OPERATOR
PARAMS         : str = config.attr.Value.Structure.PARAMS
QUERY          : str = config.attr.Value.Structure.QUERY
QUERY_FALLBACK : str = config.attr.Value.Structure.QUERY_FALLBACK
SEMANTIC_HINT  : str = config.attr.Value.Structure.SEMANTIC_HINT
SEN            : str = config.attr.Value.Structure.SEN
TAG            : str = config.attr.Value.Structure.TAG
TYPE_INSTANCE  : str = config.attr.Value.Structure.TYPE_INSTANCE
FLATTEN        : str = config.attr.Value.Structure.FLATTEN

# Core Components
QUERY_COMPONENT     : str = config.attr.Structure.Components.QUERY
TRANSFORM_COMPONENT : str = config.attr.Structure.Components.TRANSFORM
ACTION_COMPONENT    : str = config.attr.Structure.Components.ACTION

# Core data defaults

TYPE_BASE        = config.attr.Data.TYPE_BASE

# Core type primitives
COMPONENT_PRIM = config.attr.Type.Primitive.COMPONENT
CONTAINER_PRIM = config.attr.Type.Primitive.CONTAINER
OPERATOR_PRIM  = config.attr.Type.Primitive.OPERATOR_PRIM
SENTENCE_PRIM  = config.attr.Type.Primitive.SENTENCE
STRING_PRIM    = config.attr.Type.Primitive.STRING
REGEX_PRIM     = config.attr.Type.Primitive.REGEX
STRUCT_PRIM    = config.attr.Type.Primitive.STRUCTURE
