""" Simple comparison functions to be used in rules """
import logging as root_logger
from enum import Enum 
logging = root_logger.getLogger(__name__)

#comparison operators:
COMP = Enum('Comp_ops','LT GT NE EQ')


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

COMP_REVERSE_LOOKUP = {
    COMP.LT : "<",
    COMP.GT : ">",
    COMP.NE : "!=",
    COMP.EQ : "=="
}

class Comparison:

    def __init__(self, op, value=None, bind=None):
        assert(isinstance(op, COMP))
        assert(value is not None or bind is not None)
        assert(not (value is not None and bind is not None))
        self.op = op
        self.value = value
        self.bind = bind

    def copy(self):
        return Comparison(self.op, self.value, self.bind)
        
    def is_alpha_test(self):
        return self.value is not None
        
    def __repr__(self):
        if self.value is not None:
            return "{} {}".format(COMP_REVERSE_LOOKUP[self.op],
                                    self.value)
        else:
            return "{} {}".format(COMP_REVERSE_LOOKUP[self.op],
                                    repr(self.bind))
