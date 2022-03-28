"""
A DSL to generate, check, and navigate lattices

Top
Bottom
Structure

Operators: Add subtract, path to top, path to bottom

"""
from acab.core.data.value import AcabValue

class LatticeBase(AcabValue):
    """ A Description of a Lattice """

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
