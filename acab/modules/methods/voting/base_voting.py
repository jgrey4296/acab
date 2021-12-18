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
from acab.core.data.value import AcabValue

class VotingBase(AcabValue):
    """ Base Description for Voting """

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
