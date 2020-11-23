"""
A DSL to generate, check, and navigate lattices

Top
Bottom
Structure

Operators: Add subtract, path to top, path to bottom

"""
from acab.abstract.core.core_abstractions import AcabValue

class LatticeBase(AcabValue):
    """ A Description of a Lattice """

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
