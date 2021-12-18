"""
A Basic Finite State Machine Module

a.state.machine(::FSM):
	States:


	Events:


	Constraints:
	connected
end


# Query: current state, available actions
(possibly MCTS)

"""
from acab.core.data.value import AcabValue


class FSMBase(AcabValue):
    """ A Description of a Finite State Machine """

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


