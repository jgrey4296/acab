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
from py_rule.abstract.value import PyRuleValue

class VotingBase(PyRuleValue):
    """ Base Description for Voting """

    def __init__(self):
        return


    def __str__(self):
        """ Data needs to implement a str method that produces
        output that can be re-parsed """
        raise NotImplementedError()

    def copy(self):
        """ Data needs to be able to be copied """
        raise NotImplementedError()

    def bind(self, bindings):
        """ Data needs to be able to bind a dictionary
        of values to internal variables """
        raise NotImplementedError()

    def var_set(self):
        """ Data needs to be able to report internal variables """
        raise NotImplementedError()


