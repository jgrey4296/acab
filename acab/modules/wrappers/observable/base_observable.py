"""
a DSL for annotating actions to describe
how they are perceived

Action A : [
 seen : same.location?
 heard : within.distance.$x?

]


"""
from acab.core.data.values import AcabValue


class ObservableBase(AcabValue):
    """ Description of Actions and how they can be observed """

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
