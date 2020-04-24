"""
Layers can be parsable
and inserted or removed from pipelines?

should also hold tests to ensure particular results from
particular inputs, and statistical ranges

a.layer(::Layer):
    //require modules

    //rule selectors as:
    #tag_queries?
    rule.location.queries?
    rule.input.queries?
    rule.output.queries?

    + agenda(sort_by: a.query.$x?)
    + agenda
    + agenda
end

"""
from py_rule.abstract.production_operator import ProductionContainer

class Layer(ProductionContainer):
    """ The Abstract Layer Class """

    def __init__(self):
        # rule selectors are queries
        # must include a $rule binding
        # (or whatever util.LAYER_QUERY_RULE_BIND_S is)
        self._rule_selectors = []
        # TODO implement logic for this
        self._memoized_rules = []
        self._agendas = []
        self._tests = []

    def queries(self):
        return self._rule_selectors

    def agendas(self):
        return self._agendas

    def __call__(self, engine):
        """ Run a layer, returning actions to perform """
        # Run rule queries (possibly cache them)
        query_results = []

        # Run rules
        proposals = [y for x in query_results(engine) for y in x]

        # Run agendas
        selected = [agenda(proposals, engine) for agenda in self._agenda]

        return selected


#Utility function for parser
def make_layer(toks):



    return None
