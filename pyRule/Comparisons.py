from .utils import COMP


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
