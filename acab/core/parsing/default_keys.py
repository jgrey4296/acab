"""
Enforce a default set of parse structure entries to be
retrieved from config files
"""
from acab.core.config.config import GET
from enum import Enum

config = GET()

HEAD_ANNOTATION : str = config.prepare("Parse.Structure", "HEAD_ANNOTATION")()
POST_ANNOTATION : str = config.prepare("Parse.Structure", "POST_ANNOTATION")()
ACTION          : str = config.prepare("Parse.Structure", "ACTION")()
ANNOTATION      : str = config.prepare("Parse.Structure", "ANNOTATION")()
ARG             : str = config.prepare("Parse.Structure", "ARG")()
AT_BIND         : str = config.prepare("Parse.Structure", "AT_BIND")()
BIND            : str = config.prepare("Parse.Structure", "BIND")()
CONSTRAINT      : str = config.prepare("Parse.Structure", "CONSTRAINT")()
DEFAULT_ACTION  : str = config.prepare("Parse.Structure", "DEFAULT_ACTION")()
LEFT            : str = config.prepare("Parse.Structure", "LEFT")()
MODAL           : str = config.prepare("Parse.Structure", "MODAL")()
NAME            : str = config.prepare("Parse.Structure", "NAME")()
NEGATION        : str = config.prepare("Parse.Structure", "NEGATION")()
OPERATOR        : str = config.prepare("Parse.Structure", "OPERATOR")()
QUERY_FALLBACK  : str = config.prepare("Parse.Structure", "QUERY_FALLBACK")()
QUERY           : str = config.prepare("Parse.Structure", "QUERY")()
RIGHT           : str = config.prepare("Parse.Structure", "RIGHT")()
SEN             : str = config.prepare("Parse.Structure", "SEN")()
STATEMENT       : str = config.prepare("Parse.Structure", "STATEMENT")()
TAG             : str = config.prepare("Parse.Structure", "TAG")()
TARGET          : str = config.prepare("Parse.Structure", "TARGET")()
TRANSFORM       : str = config.prepare("Parse.Structure", "TRANSFORM")()
TYPE_INSTANCE   : str = config.prepare("Parse.Structure", "TYPE_INSTANCE")()
VALUE           : str = config.prepare("Parse.Structure", "VALUE")()
PARAM           : str = config.prepare("Parse.Structure", "PARAM")()
