"""
A DSL to describe multi-party interaction processes

Message formats
Addresses and Routing
Errors and Error Recovery
Sequence, Turn Taking
Prior, Post
Information flow
Queueing
Layers



"""
from py_rule.abstract.value import PyRuleValue


class ProtocolBase(PyRuleValue):
    """ Base description for an ordered protocol of actions """

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

