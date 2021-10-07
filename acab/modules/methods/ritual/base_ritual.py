"""
A DSL to describe how actions and sequences of actions
are packaged into rituals

Action Sequence As
Interleave symbolic actions from set Ss


"""
from acab.core.data.values import AcabValue

class RitualBase(AcabValue):
    """ Base Description of sequences of actions
    which do not rely on instrumental causality
    """

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
