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

from acab.core.value.value import AcabValue


class Delegation(AcabValue):
    """ A Description of a Task and its delegational possibilities """

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




class DelegationInstance(Delegation):
    """ An Instantiated Delegation """

    def __init__(self):
        return
