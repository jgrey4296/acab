""" Simple Transform funtions to be used in rules """
from .utils import TROP
import logging as root_logger
logging = root_logger.getLogger(__name__)

def ADD(a, b):
    return a + b

def SUB(a, b):
    return a - b

def MUL(a, b):
    return a * b

def DIV(a, b):
    return a / b


TROP_LOOKUP = {
    TROP.ADD : ADD,
    TROP.SUB : SUB,
    TROP.MUL : MUL,
    TROP.DIV : DIV
}
