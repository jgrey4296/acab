"""
Enforce a default set of parse structure entries to be
retrieved from config files
"""
from acab.abstract.config.config import GET

config = GET()

ACTION         : str = config.prepare("Parse.Structure", "ACTION")()
ANNOTATION     : str = config.prepare("Parse.Structure", "ANNOTATION")()
ARG            : str = config.prepare("Parse.Structure", "PARAMS")()
AT_BIND        : str = config.prepare("Parse.Structure", "AT_BIND")()
BIND           : str = config.prepare("Parse.Structure", "BIND")()
CONSTRAINT     : str = config.prepare("Parse.Structure", "CONSTRAINT")()
DEFAULT_ACTION : str = config.prepare("Parse.Structure", "DEFAULT_ACTION")()
LEFT           : str = config.prepare("Parse.Structure", "LEFT")()
MODAL          : str = config.prepare("Parse.Structure", "MODAL")()
NAME           : str = config.prepare("Parse.Structure", "NAME")()
NEGATION       : str = config.prepare("Parse.Structure", "NEGATION")()
NODE           : str = config.prepare("Parse.Structure", "NODE")()
OPERATOR       : str = config.prepare("Parse.Structure", "OPERATOR")()
QUERY_FALLBACK : str = config.prepare("Parse.Structure", "QUERY_FALLBACK")()
QUERY          : str = config.prepare("Parse.Structure", "QUERY")()
RIGHT          : str = config.prepare("Parse.Structure", "RIGHT")()
SEN            : str = config.prepare("Parse.Structure", "SEN")()
STATEMENT      : str = config.prepare("Parse.Structure", "STATEMENT")()
TAG            : str = config.prepare("Parse.Structure", "TAG")()
TARGET         : str = config.prepare("Parse.Structure", "TARGET")()
TRANSFORM      : str = config.prepare("Parse.Structure", "TRANSFORM")()
TYPE_INSTANCE  : str = config.prepare("Parse.Structure", "TYPE_INSTANCE")()
VALUE          : str = config.prepare("Parse.Structure", "VALUE")()
