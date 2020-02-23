"""
A Means of representing how physical artifacts are used
both instrumentally and symbolically in the world
and how they are specified, constrained and constructed
"""
from py_rule.abstract.value import PyRuleValue

class Artifact(PyRuleValue):
    """ Description of a physical artifact """

    def __init__(self):
        self._properties           = None
        self._constraints          = None
        self._instrumental_effects = None
        self._specifications       = None
        self._conventions          = None

    def __str__(self):
        """ Data needs to implement a str method that produces
        output that can be re-parsed """
        raise NotImplementedError()

    def copy(self):
        """ Data needs to be able to be copied """
        raise NotImplementedError()

    def bind(self, bindings):
        """ Data needs to be able to bind a dictionary
        of values to internal variables """
        raise NotImplementedError()

    def var_set(self):
        """ Data needs to be able to report internal variables """
        raise NotImplementedError()

