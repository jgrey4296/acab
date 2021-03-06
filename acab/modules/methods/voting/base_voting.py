"""
A DSL to specify multi-party resolution of disputes

see http://ncase.me/ballot/

First Past The Post
Instant Runoff
Borda
Condorcet
Approval
Score
Random
Electoral College / Hierarchy
Segregation
Suppression

"""
from acab.abstract.value import AcabValue

class VotingBase(AcabValue):
    """ Base Description for Voting """

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
