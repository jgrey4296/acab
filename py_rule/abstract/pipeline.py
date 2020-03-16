"""
As Pipelines are collections of layers,
and the layer ordering is determined by topological sorting,
a pipeline is just a set of layers that forms
a unified flow from inputs to outputs
while also storing terminals and
inter-pipeline interfaces

a.pipeline(::Pipeline):
   a.layer
   a.second.layer
   a.third.layer
end
"""


class Pipeline:
    """ Abstract Class to describe a rule engine pipeline
    Collects together sets of rules,
    and a sequence of agendas to run on their output
    """

    def __init__(self):
        self._layers = []
        # Terminals:
        self._terminal_in   = []
        self._terminal_out  = []
        self._interface_in  = []
        self._interface_out = []

    def verify(self, engine):
        """ Verify the Pipeline:
        Check all Agendas exist,
        Check rule selectors topologically sort
        Check modules are loaded
        """
        agendas = []
        for layer in self._layers:
            agendas += layer.agendas()
        # TODO verify agendas

        # TODO topologically sort rules

        # TODO check module use

        raise NotImplementedError()

    def __call__(self, engine, tick):
        """ Run this pipeline on the given engine for a tick """
        raise NotImplementedError()

    # TODO input constraints
    def input_constraints(self):
        raise NotImplementedError()

    # TODO output constraints
    def output_constraints(self):
        raise NotImplementedError()
