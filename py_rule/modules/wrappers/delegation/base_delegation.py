"""
A Means to implement a delegation logic for actions and activities

delegation can cover states or actions

state/action can be delegated,

A delegates pre[Act/State]post to B with conditions C

specified/partial/open
domain/planning
strong/weak
literal/over/critical/hyper

exploitation/induction/agreement

ability,reliability,experience,communication,trust


----------
a -> b : state

a -> b : action

a -> b : action_sequence

a -> b : state : action_set - action_set

a -> b : b.qualified.action? : action


"""

from py_rule.abstract.value import PyRuleValue


class Delegation(PyRuleValue):
    """ A Description of a Task and its delegational possibilities """

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




class DelegationInstance(Delegation):
    """ An Instantiated Delegation """

    def __init__(self):
        return

