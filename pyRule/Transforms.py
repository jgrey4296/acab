""" Simple Transform funtions to be used in rules """
from enum import Enum 
import logging as root_logger
logging = root_logger.getLogger(__name__)
from pyRule.utils import Bind

TROP = Enum("Transform_ops", "ADD SUB MUL DIV RAND RANGE REMAIN ROUND")
#todo: add regex transform

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

TROP_REVERSE_LOOKUP = {
    TROP.ADD : "+",
    TROP.SUB : "-",
    TROP.MUL : "*",
    TROP.DIV : "/",
    TROP.RAND : "RAND",
    TROP.RANGE: "RANGE",
    TROP.REMAIN : "%",
    TROP.ROUND: "_"
}

class TransformComponent:

    def __init__(self, op, source, value=None, bind=None, rebind=None):
        assert(not(value is None and bind is None))
        assert(isinstance(op, TROP))
        if bind is not None:
            assert(isinstance(source, Bind))
        if rebind is not None:
            assert(isinstance(rebind, Bind))
        assert(isinstance(op, TROP) or isinstance(op, str))
            
        self.op = op
        self.source = source
        self.val = value
        self.bind = bind
        self.rebind = rebind

    def verify_op(self):
        if self.op not in TROP_LOOKUP:
            raise Exception("Unknown Op: {}".format(self.op))
        
    def __repr__(self):
        if self.val is not None:
            rhs = self.val
        else:
            rhs = self.bind
        if self.rebind is None:
            return "{} {} {}".format(self.source, TROP_REVERSE_LOOKUP[self.op], rhs)
        else:
            return "{} {} {} -> {}".format(self.source, TROP_REVERSE_LOOKUP[self.op],
                                           rhs, self.rebind)
        
class Transform:
    #todo: add ability to select n contexts, sort contexts by parameters,
    #have min and max bounds
    def __init__(self, components):
        assert(all([isinstance(x, TransformComponent) for x in components]))
        self.components = components

    def __repr__(self):
        return "( {} )".format(",".join([repr(x) for x in self.components]))

    def verify_ops(self):
        for x in self.components:
            x.verify_op()
        
    def get_input_requirements(self):
        #return the set of input bound names
        return set([])

    def get_output_spec(self):
        #return the set of output bound names
        return set([])
