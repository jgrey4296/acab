""" Simple comparison functions to be used in rules """
from .utils import COMP
import logging as root_logger
logging = root_logger.getLogger(__name__)

def EQ(a,b):
    return a == b

def GT(a,b):
    return a > b

def LT(a,b):
    return a < b

def NEQ(a,b):
    return a != b

def NOP(a,b):
    return True

def FAIL(a,b):
    return False


COMP_LOOKUP = {
    COMP.LT : LT,
    COMP.GT : GT,
    COMP.NE : NEQ,
    COMP.EQ : EQ
}
