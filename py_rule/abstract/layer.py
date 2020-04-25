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

class Layer(Rule):
    """ The Abstract Layer Class """

    def __init__(self, query=None, action=None, transform=None, name="AnonLayer"):
        super(Layer, self).__init__(query=query,
                                    action=action,
                                    transform=transform,
                                    name=name)

    def __call__(self, engine):
        """ Run a layer, returning actions to perform """
        results = super(Layer, self).__call__(engine)

        # TODO: make 'agenda call' actions
        # run the actions
        selected = []


        return selected



#Utility function for parser
def make_layer(toks):



    return None
