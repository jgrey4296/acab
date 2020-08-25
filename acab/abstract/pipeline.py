"""
Pipelines are special cases of Rules.

The layer ordering is determined by topological sorting,
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
from acab.abstract.rule import Rule
from acab.abstract.production_operator import ProductionOperator
from acab.abstract.printing import util as PrU
from acab.config import AcabConfig

util = AcabConfig.Get()

NAME_S = util("Parsing.Structure", "NAME_S")
STATEMENT_S = util("Parsing.Structure", "STATEMENT_S")
QUERY_S = util("Parsing.Structure", "QUERY_S")
TRANSFORM_S = util("Parsing.Structure", "TRANSFORM_S")
ACTION_S = util("Parsing.Structure", "ACTION_S")

class Pipeline(Rule):
    """ Abstract Class to describe a rule engine pipeline
    Collects together sets of rules,
    and a sequence of agendas to run on their output
    """

    def __init__(self, query=None, action=None, transform=None, name="AnonPipeline"):
        super(Pipeline, self).__init__( query=query,
                                        action=action,
                                        transform=transform,
                                        name=name)
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
        """ Run this pipeline on the given engine for a tick """
        results = super(Pipeline, self).__call__(engine)

        # Run pipeline actions
        output = []

        return output

    def input_constraints(self):
        """
        TODO get the base input language of the pipeline.
        """
        raise NotImplementedError()

    def output_constraints(self):
        """
        TODO get the base output language of the pipeline
        """
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
