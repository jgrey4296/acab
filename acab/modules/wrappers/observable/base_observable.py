"""
a DSL for annotating actions to describe
how they are perceived

Action A : [
 seen : same.location?
 heard : within.distance.$x?

]


"""
from acab.abstract.core.core_abstractions import AcabValue


class ObservableBase(AcabValue):
    """ Description of Actions and how they can be observed """

    def __init__(self):
        return

    def __str__(self):
        """ Data needs to implement a str method that produces
        output that can be re-parsed """
        raise NotImplementedError()

    @property
    def var_set(self):
        """ Data needs to be able to report internal variables """
        raise NotImplementedError()


    def bind(self, bindings):
        """ Data needs to be able to bind a dictionary
        of values to internal variables """
        raise NotImplementedError()
