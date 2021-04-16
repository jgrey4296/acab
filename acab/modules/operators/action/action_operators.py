"""
Definitions of the Core Performance Operators
"""
import logging as root_logger
from enum import Enum

from acab.abstract.core.production_abstractions import ActionOperator
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
class ActionAdd(ActionOperator):

    @SemanticOperatorWrapDecorator
    def __call__(self, struct, sem, *params, data=None, semSystem=None):
        """ Assert the params (ie: sentences) """
        # TODO use override?
        semSystem(params[0])

class ActionPrint(ActionOperator):

    @SemanticOperatorWrapDecorator
    def __call__(self, *params, data=None, semSystem=None):
        """ Trigger a logging statement """
        for x in params:
            print(x)
            logging.info("Engine Output: {}".format(x))
