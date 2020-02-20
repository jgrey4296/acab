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
        # rule selector as a query?
        self._rule_selectors = []
        self._agendas = []
        self._tests = []

    def queries(self):
        return self._rule_selectors

    def agendas(self):
        return self._agendas
