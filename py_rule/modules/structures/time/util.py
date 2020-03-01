"""
Utils related to rational time
"""
from enum import Enum
from fractions import Fraction
from math import gcd
from functools import reduce
import logging as root_logger

logging = root_logger.getLogger(__name__)

PATTERN_T = Enum("Pattern Type", "DISCRETE ANALOG")
TIME_T = Enum("Time Type", "CLOCK EVENT SET SYMBOLIC")

Time = Fraction


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
