"""
These are enums and values used for AcabValue internal data attributes.

ie: AcabValue().data[BIND] = True

also also includes the modality enum, core type primitives,
and production structure keys

"""
from acab import AcabConfig
from enum import Enum

config = AcabConfig()

DATA_STRUCT_E : Enum = config.prepare("Value.Structure"      , _type=Enum)()
MODALITIES_E  : Enum = config.prepare("MODAL"                , _type=Enum)()

ARG            = DATA_STRUCT_E.ARG
AT_BIND        = DATA_STRUCT_E.AT_BIND
BIND           = DATA_STRUCT_E.BIND
CONSTRAINT     = DATA_STRUCT_E.CONSTRAINT
NEGATION       = DATA_STRUCT_E.NEGATION
PARAMS         = DATA_STRUCT_E.PARAMS
QUERY          = DATA_STRUCT_E.QUERY
QUERY_FALLBACK = DATA_STRUCT_E.QUERY_FALLBACK
SEMANTIC_HINT  = DATA_STRUCT_E.SEMANTIC_HINT
SEN            = DATA_STRUCT_E.SEN
TAG            = DATA_STRUCT_E.TAG
TYPE_INSTANCE  = DATA_STRUCT_E.TYPE_INSTANCE
FLATTEN        = DATA_STRUCT_E.FLATTEN

# Core elements of value data
# TODO move this out of Value.Structure
OPERATOR       : str = config.attr.Value.Structure.OPERATOR

# TODO refactor to use this instead
STRUCT_COMP_E : Enum = config.prepare("Structure.Components" , _type=Enum)()
# Core Components
QUERY_COMPONENT     : str = config.attr.Structure.Components.QUERY
TRANSFORM_COMPONENT : str = config.attr.Structure.Components.TRANSFORM
ACTION_COMPONENT    : str = config.attr.Structure.Components.ACTION

# Core data defaults

TYPE_BASE        = config.attr.Data.TYPE_BASE

# TODO refactor to use this instead
TYPE_PRIM_E   : Enum = config.prepare("Type.Primitive"       , _type=Enum)()
# Core type primitives
COMPONENT_PRIM = config.attr.Type.Primitive.COMPONENT
CONTAINER_PRIM = config.attr.Type.Primitive.CONTAINER
OPERATOR_PRIM  = config.attr.Type.Primitive.OPERATOR_PRIM
SENTENCE_PRIM  = config.attr.Type.Primitive.SENTENCE
STRING_PRIM    = config.attr.Type.Primitive.STRING
REGEX_PRIM     = config.attr.Type.Primitive.REGEX
STRUCT_PRIM    = config.attr.Type.Primitive.STRUCTURE
INSTR_PRIM     = config.attr.Type.Primitive.INSTRUCT
