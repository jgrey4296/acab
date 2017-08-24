""" Simple Transform funtions to be used in rules """
from enum import Enum 
import logging as root_logger
from pyRule.utils import Bind
from random import uniform, sample, randint
from math import floor
from re import sub
import IPython
logging = root_logger.getLogger(__name__)

TROP = Enum("Transform_ops", "ADD SUB MUL DIV RAND REMAIN ROUND NEG REGEX FORMAT SELECT SELECT_ALL")
#todo: add regex transform

def ADD(a, b):
    return a + b

def SUB(a, b):
    return a - b

def MUL(a, b):
    return a * b

def DIV(a, b):
    return a / b

def RAND(a, b):
    #Rand between 0 and 1
    return uniform(a, b)

def REMAIN(a, b):
    #divde and get remainder?
    raise Exception("Not implemented yet")

def ROUND(a, data):
    #round to integer
    return floor(a)

def NEG(a, data):
    #invert the number
    return -a

def REGEX(a, b, data):
    #substitute a pattern with a value from passed in data
    return sub(b, a, data)

def FORMAT(a, data):
    #use str.format variant with a data dictionary
    return a.format(**data)

def SELECT(alts, min, max):
    return sample(alts, randomint(min, max))


TROP_LOOKUP = {
    TROP.ADD : ADD,
    TROP.SUB : SUB,
    TROP.MUL : MUL,
    TROP.DIV : DIV,
    TROP.RAND : RAND,
    TROP.REMAIN : REMAIN,
    TROP.ROUND : ROUND,
    TROP.NEG : NEG,
    TROP.REGEX : REGEX,
    TROP.FORMAT: FORMAT,
    TROP.SELECT : SELECT    
}

TROP_REVERSE_LOOKUP = {
    TROP.ADD : "+",
    TROP.SUB : "-",
    TROP.MUL : "*",
    TROP.DIV : "/",
    TROP.RAND : "<->",
    TROP.REMAIN : "%",
    TROP.ROUND: "_",
    TROP.NEG : "-",
    TROP.REGEX : "~="  ,
    TROP.FORMAT : "~{}",
    TROP.SELECT : "select"
}

TROP_PARAM_LENGTHS = {
    TROP.ADD : 2,
    TROP.SUB : 2,
    TROP.MUL : 2,
    TROP.DIV : 2,
    TROP.RAND : 2,
    TROP.REMAIN : 2,
    TROP.ROUND: 1,
    TROP.NEG : 1,
    TROP.REGEX : 3,
    TROP.FORMAT: 1,
    TROP.SELECT : 2
}


class TransformComponent:
    """ Superclass of Transforms """
    def __init__(self, op):
        assert(isinstance(op, TROP))
        self.op = op
    
class SelectionTransform(TransformComponent):
    def __init__(self, lBound, uBound, op=TROP.SELECT):
        super().__init__(op)
        self.lBound = lBound
        self.uBound = uBound

    def __repr__(self):
        if self.lBound is TROP.SELECT_ALL:
            lbound = "_"
        elif isinstance(self.lBound, Bind):
            lbound = repr(self.lBound)
        else:
            lbound = str(self.lBound)
            
        if self.uBound is TROP.SELECT_ALL:
            ubound = "_"
        elif isinstance(self.uBound, Bind):
            ubound = repr(self.uBound)
        else:
            ubound = str(self.uBound)
            
        return "select {} - {}".format(lbound, ubound)

        
class OperatorTransform(TransformComponent):
    def __init__(self, op, source, value=None, bind=None, rebind=None):
        super().__init__(op)
        if bind is not None:
            assert(isinstance(source, Bind))
        if rebind is not None:
            assert(isinstance(rebind, Bind))
        assert(isinstance(op, TROP) or isinstance(op, str))
            
        self.source = source
        self.val = value
        self.bind = bind
        self.rebind = rebind

    def verify_op(self):
        if self.op not in TROP_LOOKUP:
            raise Exception("Unknown Op: {}".format(self.op))

    def setRebind(self, bind):
        assert(isinstance(bind, Bind))
        assert(self.rebind is None)
        self.rebind = bind
        
    def __repr__(self):
        op = TROP_REVERSE_LOOKUP[self.op]
        source = repr(self.source)
        if self.rebind is not None:
            rebind = " -> {}".format(repr(self.rebind))
        else:
            rebind = ""
        if self.val is not None and isinstance(self.val, float):
            value = str(self.val)
            value = value.replace(".","d")
        elif self.val is not None:
            value = str(self.val)
        else:
            value = ""
        if self.bind is not None:
            bind = repr(self.bind)
        else:
            bind = ""
            
        assert(self.op in TROP_PARAM_LENGTHS)
        param_length = TROP_PARAM_LENGTHS[self.op]
        if param_length == 1:
            return "{}{}{}".format(op, source, rebind)
        elif param_length == 2:
            return "{} {} {}{}{}".format(source, op, value, bind, rebind)
        elif param_length == 3:
            if bind != '':
                bind = ' ' + bind
            return "{} {} /{}/{}{}".format(source, op, value, bind, rebind)

        
class Transform:
    #todo: add ability to select n contexts, sort contexts by parameters,
    #have min and max bounds
    def __init__(self, components):
        assert(all([isinstance(x, TransformComponent) for x in components]))
        selection = [x for x in components if isinstance(x, SelectionTransform)]
        if len(selection) == 1:
            self.selection = selection[0]
        else:
            self.selection = None
        self.components = [x for x in components if not isinstance(x, SelectionTransform)]


    def __repr__(self):
        if self.selection is not None:
            sel = repr(self.selection)
        else:
            sel = ""
        
        return "{}{}".format(sel,", ".join([repr(x) for x in self.components]))

    def verify_ops(self):
        for x in self.components:
            x.verify_op()

    def getSelectionBounds(self):
        if self.selection is None:
            return (None, None)
        else:
            return (self.selection.lBound, self.selection.uBound)            
            
    def get_input_requirements(self):
        #return the set of input bound names
        return set([])

    def get_output_spec(self):
        #return the set of output bound names
        return set([])
