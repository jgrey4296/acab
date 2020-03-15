"""
The Base Operator Definition
Used for Comparison, Transform, and Performance Operators
"""
from .value import PyRuleValue
from .sentence import Sentence

class ProductionOperator(PyRuleValue):
    """ The Base Operator Class """
    # operator calls are late resolving,
    # and allow type checking to resolve earlier

    def __init__(self, num_params=2, infix=False):
        self._op_str = self.__class__.__name__
        self._num_params = num_params
        self._infix = infix

    def __call__(self):
        raise NotImplementedError()

    def __str__(self):
        raise NotImplementedError()

    def __repr__(self):
        raise NotImplementedError()
