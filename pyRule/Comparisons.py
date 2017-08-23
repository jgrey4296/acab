""" Simple comparison functions to be used in rules """
import logging as root_logger
from enum import Enum 
logging = root_logger.getLogger(__name__)

#comparison operators:
#todo: add regex string match comparison
COMP = Enum('Comp_ops','LT GT NE EQ RE')


def EQ(a,b):
    return a == b

def GT(a,b):
    return a > b

def LT(a,b):
    return a < b

def NEQ(a,b):
    return a != b

def REGMATCH(a,b):
    return re.search(b, a)


COMP_LOOKUP = {
    COMP.LT : LT,
    COMP.GT : GT,
    COMP.NE : NEQ,
    COMP.EQ : EQ,
    COMP.RE : REGMATCH
    
}

COMP_REVERSE_LOOKUP = {
    COMP.LT : "<",
    COMP.GT : ">",
    COMP.NE : "!=",
    COMP.EQ : "==",
    COMP.RE : "~="
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

    def is_regex_test(self):
        return self.op is COMP.RE
    
    def __repr__(self):
        if self.is_regex_test():
            val = "/{}/".format(self.value)
        else:
            val = self.value
            
        if self.value is not None:
            return "{} {}".format(COMP_REVERSE_LOOKUP[self.op],
                                    val)
        else:
            return "{} {}".format(COMP_REVERSE_LOOKUP[self.op],
                                    repr(self.bind))
