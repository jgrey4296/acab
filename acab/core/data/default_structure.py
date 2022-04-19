"""
Default elements found in values, loaded from config
"""
from acab.core.config.config import GET
from enum import Enum

config = GET()

# TODO refactor to use these instead
DATA_STRUCT_E : Enum = config.prepare("Value.Structure", as_enum=True)()
STRUCT_COMP_E : Enum = config.prepare("Structure.Components", as_enum=True)()
TYPE_PRIM_E   : Enum = config.prepare("Type.Primitive", as_enum=True)()
MODALITIES_E  : Enum = config.prepare("MODAL", as_enum=True)()

# Core elements of value data
AT_BIND        : str = config.prepare("Value.Structure", "AT_BIND")()
BIND           : str = config.prepare("Value.Structure", "BIND")()
CONSTRAINT     : str = config.prepare("Value.Structure", "CONSTRAINT")()
NEGATION       : str = config.prepare("Value.Structure", "NEGATION")()
OPERATOR       : str = config.prepare("Value.Structure", "OPERATOR")()
PARAMS         : str = config.prepare("Value.Structure", "PARAMS")()
QUERY          : str = config.prepare("Value.Structure", "QUERY")()
QUERY_FALLBACK : str = config.prepare("Value.Structure", "QUERY_FALLBACK")()
SEMANTIC_HINT  : str = config.prepare("Value.Structure", "SEMANTIC_HINT")()
SEN            : str = config.prepare("Value.Structure", "SEN")()
TAG            : str = config.prepare("Value.Structure", "TAG")()
TYPE_INSTANCE  : str = config.prepare("Value.Structure", "TYPE_INSTANCE")()
FLATTEN        : str = config.prepare("Value.Structure", "FLATTEN")()

# Core Components
QUERY_COMPONENT     : str = config.prepare("Structure.Components", "QUERY")()
TRANSFORM_COMPONENT : str = config.prepare("Structure.Components", "TRANSFORM")()
ACTION_COMPONENT    : str = config.prepare("Structure.Components", "ACTION")()

# Core data defaults
ROOT             = config.prepare("Data", "ROOT")()
TYPE_BOTTOM_NAME = config.prepare("Data", "TYPE_BOTTOM_NAME")()

# Core type primitives
COMPONENT_PRIM = config.prepare("Type.Primitive", "COMPONENT")()
CONTAINER_PRIM = config.prepare("Type.Primitive", "CONTAINER")()
OPERATOR_PRIM  = config.prepare("Type.Primitive", "OPERATOR")()
SENTENCE_PRIM  = config.prepare("Type.Primitive", "SENTENCE")()
STRING_PRIM    = config.prepare("Type.Primitive", "STRING")()
REGEX_PRIM     = config.prepare("Type.Primitive", "REGEX")()
STRUCT_PRIM    = config.prepare("Type.Primitive", "STRUCTURE")()
