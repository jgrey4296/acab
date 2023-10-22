"""
These are strings to be used as parts of parse results

ie: pp.Literal("something)(HEAD_ANNOTATION) + pp.Literal("else")(POST_ANNOTATION)
etc

"""
##-- imports
import acab
from enum import Enum

##-- end imports

config = acab.config

# These have to be strings, as pyparsing can't name a parser with an enum
#
key_dict = {x:x for x in config.all_of().parse.results.structure()}

HEAD_ANNOTATION : str = key_dict.get(HEAD_ANNOTATION, "HEAD_ANNOTATION")
POST_ANNOTATION : str = key_dict.get(POST_ANNOTATION, "POST_ANNOTATION")
ACTION          : str = key_dict.get(ACTION, "ACTION")
ANNOTATION      : str = key_dict.get(ANNOTATION, "ANNOTATION")
ARG             : str = key_dict.get(ARG, "ARG")
AT_BIND         : str = key_dict.get(AT_BIND, "AT_BIND")
BIND            : str = key_dict.get(BIND, "BIND")
CONSTRAINT      : str = key_dict.get(CONSTRAINT, "CONSTRAINT")

DEFAULT_ACTION  : str = key_dict.get(DEFAULT_ACTION, "DEFAULT_ACTION")
LEFT            : str = key_dict.get(LEFT, "LEFT")
MODAL           : str = key_dict.get(MODAL, "MODAL")
NAME            : str = key_dict.get(NAME, "NAME")
NEGATION        : str = key_dict.get(NEGATION, "NEGATION")
OPERATOR        : str = key_dict.get(OPERATOR, "OPERATOR")
QUERY_FALLBACK  : str = key_dict.get(QUERY_FALLBACK, "QUERY_FALLBACK")
QUERY           : str = key_dict.get(QUERY, "QUERY")
RIGHT           : str = key_dict.get(RIGHT, "RIGHT")
SEN             : str = key_dict.get(SEN, "SEN")
STATEMENT       : str = key_dict.get(STATEMENT, "STATEMENT")
TAG             : str = key_dict.get(TAG, "TAG")
TARGET          : str = key_dict.get(TARGET, "TARGET")
TRANSFORM       : str = key_dict.get(TRANSFORM, "TRANSFORM")
TYPE_INSTANCE   : str = key_dict.get(TYPE_INSTANCE, "TYPE_INSTANCE")
VALUE           : str = key_dict.get(VALUE, "VALUE")
PARAM           : str = key_dict.get(PARAM, "PARAM")
