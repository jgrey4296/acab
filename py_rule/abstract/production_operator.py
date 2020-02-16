"""
The Base Operator Definition
Used for Comparison, Transform, and Performance Operators
"""
from .value import PyRuleValue

class ProductionOperator(PyRuleValue):
    """ The Base Operator Class """

    def __init__(self):
        self._op_str = None
        return

    def __call__(self):
        return

    def __str__(self):
        raise Exception("Abstract str method")

    def __repr__(self):
        raise Exception("Abstract repr method")
