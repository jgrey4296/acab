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
from py_rule.abstract.production_operator import ProductionContainer

class Pipeline(ProductionContainer):
    """ Abstract Class to describe a rule engine pipeline
    Collects together sets of rules,
    and a sequence of agendas to run on their output
    """

    def __init__(self):
        # Layers in the main loop
        self._layers = []

        # Free layers to call manually.
        # Eg: analysis
        self._free_layers = []

        # Terminals:
        # constructed from flow analysis
        self._terminal_in   = []
        self._terminal_out  = []
        self._interface_in  = []
        self._interface_out = []

        self._layer_dict = None

    def __getitem__(self, i):
        if self._layer_dict is None:
            self._layer_dict = {x.__name__ : x for x in self._layers}
        return self._layer_dict[i]

    def __call__(self, engine, tick):
        """ Run this pipeline on the given engine for a tick """
        raise NotImplementedError()

    # TODO input constraints
    def input_constraints(self):
        raise NotImplementedError()

    # TODO output constraints
    def output_constraints(self):
        raise NotImplementedError()

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
