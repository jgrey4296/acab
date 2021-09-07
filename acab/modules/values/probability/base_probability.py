"""
A Probabilistic programming DSL
Bayesian networks?

Condition
Distribution


"""
from acab.abstract.core.values import AcabValue

class ProbabilityBase(AcabValue):
    """ Base Class for Probabilistic programming and calculations """

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
