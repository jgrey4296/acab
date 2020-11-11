"""
Layers are a special case of Rule.
They query for rules, then run them,
passing the results to an agenda.

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

# TODO specify working memory?
"""
import logging as root_logger

from acab.abstract.rule.rule import Rule
from acab.abstract.rule.production_operator import ProductionOperator, ProductionContainer
from acab.abstract.config.config import AcabConfig


util = AcabConfig.Get()

NAME_S      = util.value("Parse.Structure", "NAME")
STATEMENT_S = util.value("Parse.Structure", "STATEMENT")
QUERY_S     = util.value("Parse.Structure", "QUERY")
TRANSFORM_S = util.value("Parse.Structure", "TRANSFORM")
ACTION_S    = util.value("Parse.Structure", "ACTION")


logging = root_logger.getLogger(__name__)


class Layer(Rule):
    """ The Abstract Layer Class """

    def __init__(self, query=None, action=None, transform=None, name="AnonLayer"):
        super(Layer, self).__init__(query=query,
                                    action=action,
                                    transform=transform,
                                    name=name)

    def __call__(self, ctxs=None, engine=None):
        """ Run a layer, returning actions to perform """
        # rule returns [(data,self)]
        results = super(Layer, self).__call__(ctxs=ctxs, engine=engine)

        logging.warning("Layer results: {}".format(len(results) < 1))

        # Run layer actions
        contexts = []
        if bool(results):
            contexts.append(results[0][0])

        action_results = self._action(ctxs=contexts, engine=engine)

        return action_results



