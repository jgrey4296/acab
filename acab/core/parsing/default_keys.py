"""
Enforce a default set of parse structure entries to be
retrieved from config files
"""
from acab import AcabConfig
from enum import Enum

config = AcabConfig()

HEAD_ANNOTATION : str = config.attr.Parse.Structure.HEAD_ANNOTATION
POST_ANNOTATION : str = config.attr.Parse.Structure.POST_ANNOTATION
ACTION          : str = config.attr.Parse.Structure.ACTION
ANNOTATION      : str = config.attr.Parse.Structure.ANNOTATION
ARG             : str = config.attr.Parse.Structure.ARG
AT_BIND         : str = config.attr.Parse.Structure.AT_BIND
BIND            : str = config.attr.Parse.Structure.BIND
CONSTRAINT      : str = config.attr.Parse.Structure.CONSTRAINT
DEFAULT_ACTION  : str = config.attr.Parse.Structure.DEFAULT_ACTION
LEFT            : str = config.attr.Parse.Structure.LEFT
MODAL           : str = config.attr.Parse.Structure.MODAL
NAME            : str = config.attr.Parse.Structure.NAME
NEGATION        : str = config.attr.Parse.Structure.NEGATION
OPERATOR        : str = config.attr.Parse.Structure.OPERATOR
QUERY_FALLBACK  : str = config.attr.Parse.Structure.QUERY_FALLBACK
QUERY           : str = config.attr.Parse.Structure.QUERY
RIGHT           : str = config.attr.Parse.Structure.RIGHT
SEN             : str = config.attr.Parse.Structure.SEN
STATEMENT       : str = config.attr.Parse.Structure.STATEMENT
TAG             : str = config.attr.Parse.Structure.TAG
TARGET          : str = config.attr.Parse.Structure.TARGET
TRANSFORM       : str = config.attr.Parse.Structure.TRANSFORM
TYPE_INSTANCE   : str = config.attr.Parse.Structure.TYPE_INSTANCE
VALUE           : str = config.attr.Parse.Structure.VALUE
PARAM           : str = config.attr.Parse.Structure.PARAM
