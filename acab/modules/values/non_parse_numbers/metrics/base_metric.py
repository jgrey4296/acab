"""
A means to describe various metrics,
 how to commensurate different values,

distance
vectors


"""
from acab.abstract.core.value import AcabValue

class MetricBase(AcabValue):
    """ A Base Class for a means of assessing and comparing  """

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
