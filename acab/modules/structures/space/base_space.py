"""
A simple module to specify locations, and pathfind between them

Space:
locations:
connections:


"""
from acab.core.data.value import AcabValue

class SpatialBase:
    """ Base Description of a spatial logic """

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
