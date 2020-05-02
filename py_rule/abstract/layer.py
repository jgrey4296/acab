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
from py_rule.abstract.rule import Rule
from py_rule.abstract.production_operator import ProductionOperator
from py_rule import util


class LayerAction(ProductionOperator):
    """ Subclass this to make layer actions """

    op_list = {}

    def __init__(self, num_params=0):
        super(LayerAction, self).__init__(num_params=num_params)

        if self.op_str not in LayerAction.op_list:
            LayerAction.op_list[self.op_str] = self


class Layer(Rule):
    """ The Abstract Layer Class """

    def __init__(self, query=None, action=None, transform=None, name="AnonLayer"):
        super(Layer, self).__init__(query=query,
                                    action=action,
                                    transform=transform,
                                    name=name)
        self._data[util.OP_CLASS_S] = LayerAction

    def __call__(self, ctxs=None, engine=None):
        """ Run a layer, returning actions to perform """
        # rule returns [(data,self)]
        results = super(Layer, self).__call__(ctxs=ctxs, engine=engine)

        assert(len(results), 1)

        # Run layer actions
        action_results = self._action(ctxs=[results[0][0]], engine=engine)

        return selected



#Utility function for parser
def make_layer(toks):



    return None
