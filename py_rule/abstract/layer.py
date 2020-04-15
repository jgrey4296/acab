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


class Layer:
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



#Utility function for parser
def make_layer(toks):



    return None
