"""
Definitions of the Core Performance Operators
"""
import logging as root_logger
from enum import Enum

from acab.abstract.core.production_abstractions import ActionOperator, ProductionOperator
from acab.abstract.decorators.semantic import OperatorArgUnWrap, OperatorResultWrap

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

    def __call__(self, *params, data=None, semSystem=None):
        """ Assert the params (ie: sentences) """
        # TODO use override?
        # TODO enable queing?
        semSystem(params[0])

class ActionPrint(ActionOperator):

    @OperatorArgUnWrap
    def __call__(self, *params, data=None, semSystem=None):
        """ Trigger a logging statement """
        # TODO bind the string
        printer = semSystem._data['printer']
        total = ""
        for x in params:
            total += printer.pprint(x)

        print(total)

class ActionUpdateOperators(ActionOperator):

    def __call__(self, *params, data=None, semSystem=None):
        """ λUpdateOps "+" $x
        Updates the Sem System's Operator cache
        """
        logging.info("Updating Operator Cache")
        cache = semSystem._operator_cache
        operator = cache[params[1]]
        new_cache = cache.bind_dict({str(params[0]) : operator})
        semSystem._operator_cache = new_cache
