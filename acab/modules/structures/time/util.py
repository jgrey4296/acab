"""
Utils related to rational time
"""
from acab import util
from enum import Enum
from fractions import Fraction
from math import gcd
from functools import reduce
import logging as root_logger

logging = root_logger.getLogger(__name__)


#CONSTANTS:
BIND_S         = util.BIND_S
NAME_S         = util.NAME_S
OPT_S          = "opt"
PATTERN_S      = "pattern"
PATTERN_T      = Enum("Pattern Type", "DISCRETE ANALOG")
TIME_T         = Enum("Time Type", "CLOCK EVENT SET SYMBOLIC")
Time           = Fraction
VALUE_S        = util.VALUE_S
VALUE_TYPE_S   = util.VALUE_TYPE_S
TIME_EVENT_S   = "event"
TIME_PATTERN_S = "pattern"

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
    return "{}/{}".format(time.numerator, time.denominator)



# Printing utilities
