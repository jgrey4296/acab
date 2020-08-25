"""
Utils related to rational time
"""
from enum import Enum
from fractions import Fraction
from math import gcd
from functools import reduce
import logging as root_logger

from acab.abstract.type_base import TypeInstance
from acab.config import AcabConfig

logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()

#CONSTANTS:
BIND_S         = util("Parsing.Structure", "BIND_S")
NAME_S         = util("Parsing.Structure", "NAME_S")
VALUE_S        = util("Parsing.Structure", "VALUE_S")
VALUE_TYPE_S   = util("Parsing.Structure", "VALUE_TYPE_S")

OPT_S          = util("Module.Time", "OPT_S")
PATTERN_S      = util("Module.Time", "PATTERN_S")
TIME_EVENT_S   = util("Module.Time", "TIME_EVENT_S")
TIME_PATTERN_S = util("Module.Time", "TIME_PATTERN_S")
TIME_FORMAT_S  = util("Module.Time", "TIME_FORMAT_S")

PATTERN_T      = Enum("Pattern Type", "DISCRETE ANALOG")
TIME_T         = Enum("Time Type", "CLOCK EVENT SET SYMBOLIC")
Time           = Fraction
# Primitive Type instances
# EVENT
# DURATION
# PATTERN

def lcm(a, b):
    """Return lowest common multiple.
    from https://stackoverflow.com/questions/49981286
    """
    if isinstance(a, Fraction):
        denom = f_gcd(a, b)
    else:
        denom = gcd(a, b)

    return a * b // denom


def f_gcd(x, y):
    """ Get the Greatest Common Divisor for two rational numbers
    from https://stackoverflow.com/questions/49981286
    """
    a = x.numerator
    b = x.denominator
    c = y.numerator
    d = y.denominator
    return Fraction(gcd(a, c), lcm(b, d))


def time_str(time):
    return TIME_FORMAT_S.format(time.numerator, time.denominator)



# TODO Printing utilities
