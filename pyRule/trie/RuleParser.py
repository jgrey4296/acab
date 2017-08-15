import logging as root_logger
import pyparsing as pp
from . import FactParser as FP
from . import QueryParser as QP
from . import TransformParser as TP
from . import ActionParser as AP

logging = root_logger.getLogger(__name__)

s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))

#todo: a parser to combine { query -> transform -> action }


def parseString(s):
    return s

def parseStrings(s):
    return s


"""
Rules as Fact Strings, so:
.a.rule.?.a.b.$x(>20)!$z(!=x)
.a.rule.?.~.a.b.c
.a.rule.~>.($x + 20 -> $y, $z * 2)
.a.rule.=>(+.a.b.$x, -.a.b.$z, print($z))


"""
