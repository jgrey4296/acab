"""
A DSL to describe how actions and sequences of actions
are packaged into rituals

Action Sequence As
Interleave symbolic actions from set Ss


"""
from py_rule.abstract.value import PyRuleValue

class RitualBase(PyRuleValue):
    """ Base Description of sequences of actions
    which do not rely on instrumental causality
    """

    def __init__(self):
        return

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




