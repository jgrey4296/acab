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
from py_rule.abstract.rule import Rule
from py_rule.abstract.production_operator import ProductionOperator
from py_rule.util import NAME_S, STATEMENT_S, TYPE_DEC_S, QUERY_S, TRANSFORM_S, ACTION_S
from py_rule import util
from py_rule.abstract.printing import util as PrU

class Pipeline(Rule):
    """ Abstract Class to describe a rule engine pipeline
    Collects together sets of rules,
    and a sequence of agendas to run on their output
    """

    def __init__(self, query=None, action=None, transform=None, name="AnonPipeline"):
        super(Pipeline, self).__init__( query=query,
                                        action=action,
                                        transform=transform,
                                        name=name,
                                        type_str=util.PIPE_HEAD_S)
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

    def __call__(self, engine):
        # TODO
        """ Run this pipeline on the given engine for a tick """
        results = super(Pipeline, self).__call__(engine)

        # Run pipeline actions
        output = []

        return output

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



def make_pipeline(toks):
    # Get Conditions
    if QUERY_S in toks:
        c = toks[QUERY_S][0][1]
        assert(isinstance(c, ProductionContainer))
    else:
        c = None

    # Get Transform
    if TRANSFORM_S in toks:
        t = toks[TRANSFORM_S][0][1]
        assert(isinstance(t, ProductionContainer))
    else:
        t = None

    # Get Action
    if ACTION_S in toks:
        a = toks[ACTION_S][0][1]
        assert(isinstance(a, ProductionContainer))
    else:
        a = None

    the_pipeline = Pipeline(query=c, transform=t, action=t)

    return (the_pipeline.type, the_layer)


PrU.register_statement({util.PIPE_HEAD_S : util.PIPE_HEAD_S})
