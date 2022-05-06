"""
A Means to specify and use easings and curves
"""
from acab.core.value.value import AcabValue

class Curve(AcabValue):
    """ The Base definition of a curve / easing """

    def __init__(self):
        return

    def __str__(self):
        """ Data needs to implement a str method that produces
        output that can be re-parsed """
        raise NotImplementedError()

    def bind(self, bindings):
        """ Data needs to be able to bind a dictionary
        of values to internal variables """
        raise NotImplementedError()


class SValCurve(AcabValue):
    """ A non-parseable Single Value to move along a curve """
    pass

class MValCurve(AcabValue):
    """ A non-parseable Distribution of values along a curve to sample from """
    pass


# Operators:
# sample from
# move along
