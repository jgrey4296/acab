"""
A Means to describe and work with random number generators

Distributions
Range
Seed

TODO should actually deal with marking indeterminacy in actions
ie: Malaby's categories: Formal, Social, Performative and Cosmological

"""
from acab.abstract.core.core_abstractions import AcabValue

class IndeterminacyBase(AcabValue):
    """ Base Source of Indeterminacy.
    Random numbers, distributions etc"""

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

    def var_set(self):
        """ Data needs to be able to report internal variables """
        raise NotImplementedError()




