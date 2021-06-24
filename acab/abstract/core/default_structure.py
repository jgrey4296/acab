from acab.abstract.config.config import GET

config = GET()

# Core elements of value data
AT_BIND        : str = config.value("Value.Structure", "AT_BIND")
BIND           : str = config.value("Value.Structure", "BIND")
CONSTRAINT     : str = config.value("Value.Structure", "CONSTRAINT")
NEGATION       : str = config.value("Value.Structure", "NEGATION")
OPERATOR       : str = config.value("Value.Structure", "OPERATOR")
PARAMS         : str = config.value("Value.Structure", "PARAMS")
QUERY          : str = config.value("Value.Structure", "QUERY")
QUERY_FALLBACK : str = config.value("Value.Structure", "QUERY_FALLBACK")
SEMANTIC_HINT  : str = config.value("Value.Structure", "SEMANTIC_HINT")
SEN            : str = config.value("Value.Structure", "SEN")
TAG            : str = config.value("Value.Structure", "TAG")
TYPE_INSTANCE  : str = config.value("Value.Structure", "TYPE_INSTANCE")

# Core data defaults
ROOT             = config.value("Data", "ROOT")
TYPE_BOTTOM_NAME = config.value("Data", "TYPE_BOTTOM_NAME")

# Core type primitives
COMPONENT_PRIM = config.value("Type.Primitive", "COMPONENT")
CONTAINER_PRIM = config.value("Type.Primitive", "CONTAINER")
OPERATOR_PRIM  = config.value("Type.Primitive", "OPERATOR")
SENTENCE_PRIM  = config.value("Type.Primitive", "SENTENCE")
STRING_PRIM    = config.value("Type.Primitive", "STRING")
REGEX_PRIM     = config.value("Type.Primitive", "REGEX")
