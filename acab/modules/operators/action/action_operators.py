"""
Definitions of the Core Performance Operators
"""
import logging as root_logger
from enum import Enum

from acab.abstract.core.production_abstractions import ProductionOperator
from acab.modules.semantics.util import SemanticOperatorWrapDecorator

logging = root_logger.getLogger(__name__)

# Action operators:

# TODO: add rule modification actions:
# add/remove penumbral condition
# add/remove penumbral action
# add/remove tag?
# insert/remove penumbral action from sequence
# modify hierarchy

# TODO action operators joots
# and/or return instructions for the semantic system
class ActionAdd(ProductionOperator):

    @SemanticOperatorWrapDecorator
    def __call__(self, struct, sem, *params, data=None):
        """ Assert the params into the engine """
        # assert(all([isinstance(x, Node) for x in params[0]]))
        engine.add(params[0])

class ActionPrint(ProductionOperator):

    @SemanticOperatorWrapDecorator
    def __call__(self, *params, data=None):
        """ Trigger a logging statement """
        for x in params:
            print(x)
            logging.info("Engine Output: {}".format(x))
