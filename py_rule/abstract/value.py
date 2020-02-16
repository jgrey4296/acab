"""
The Core Value Class
"""

class PyRuleValue:

    def __init__(self):
        return

    def __str__(self):
        """ Data needs to implement a str method that produces
        output that can be re-parsed """
        raise Exception("This is an Abstract Method")

    def copy(self):
        """ Data needs to be able to be copied """
        raise Exception("This is an Abstract Method")

    def bind(self, bindings):
        """ Data needs to be able to bind a dictionary
        of values to internal variables """
        raise Exception("This is an Abstract Method")

    def var_set(self):
        """ Data needs to be able to report internal variables """
        raise Exception("This is an Abstract Method")

