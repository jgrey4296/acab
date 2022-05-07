"""
Utils related to rational time
"""
from enum import Enum
from fractions import Fraction
from math import gcd
from functools import reduce
import logging as logmod

from acab.core.value.sentence import Sentence
from acab.core.config.config import AcabConfig
import acab.core.value.default_structure as DS

logging = logmod.getLogger(__name__)

config = AcabConfig()

TIME_FORMAT_S   = config.prepare("Modules.Time", "TIME_FORMAT")()

#CONSTANTS:
BIND_S          = DS.BIND
NAME_S          = DS.NAME
TYPE_INSTANCE_S = DS.TYPE_INSTANCE


VALUE_S         = config.prepare("Parse.Structure", "VALUE")()
OPT_S           = config.prepare("Parse.Structure", "OPT")()
PATTERN_S       = config.prepare("Parse.Structure", "PATTERN")()
TIME_EVENT_S    = config.prepare("Parse.Structure", "TIME_EVENT")()
TIME_PATTERN_S  = config.prepare("Parse.Structure", "TIME_PATTERN")()

PATTERN_T       = Enum("Pattern Type", "DISCRETE ANALOG")
TIME_T          = Enum("Time Type", "CLOCK EVENT SET SYMBOLIC")
Time            = Fraction
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
