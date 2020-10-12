"""
A Means to specify and analyse the flow of a resource through a system

Source:
Sink:
Paths:


"""
from acab.abstract.core.value import AcabValue

class FlowBase(AcabValue):
    """ The Base class of a flow analysis """

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


class FlowSource(FlowBase):
    """ A Source Node in a Flow Graph """

    def __init__(self):
        return


class FlowTransport(FlowBase):
    """ An Intermediary Node in a Flow Graph """

    def __init__(self):
        return


class FlowSink(FlowBase):
    """ A Sink Node in a Flow Graph """

    def __init__(self):
        return
