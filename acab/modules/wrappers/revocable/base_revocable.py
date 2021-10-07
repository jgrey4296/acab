"""
A DSL to describe how permissions can be given and revoked

Deontic operators
permission, requirements
State vs Action

Give,
Give Conditionally,
Give with reciprocation
Reciprocate
Retract
Give for a time period


"""

from acab.core.data.values import AcabValue

class RevocationBase(AcabBase):
    """ Base description of revocable permissions """

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

