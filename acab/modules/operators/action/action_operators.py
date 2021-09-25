"""
Definitions of the Core Performance Operators
"""
import logging as root_logger
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.abstract.core.production_abstractions import (ActionOperator,
                                                        ProductionOperator)
from acab.abstract.decorators.semantic import (OperatorArgUnWrap,
                                               OperatorResultWrap)
from acab.error.acab_semantic_exception import AcabSemanticException

logging = root_logger.getLogger(__name__)

Sentence = "Sentence"

# Action operators:

# TODO: add rule modification actions:
# add/remove penumbral condition
# add/remove penumbral action
# add/remove tag?
# insert/remove penumbral action from sequence
# modify hierarchy

# TODO action operators joots
# and/or return instructions for the semantic system
class AcabAssert(ActionOperator):

    def __call__(self, *params, data=None, semSystem=None):
        """ Assert the params (ie: sentences) """
        # TODO use override?
        # TODO enable queing?
        semSystem(params[0])

class AcabPrint(ActionOperator):

    @OperatorArgUnWrap
    def __call__(self, *params, data=None, semSystem=None):
        """ Trigger a logging statement """
        # TODO bind the string
        printer = semSystem._data['printer']
        total = ""
        for x in params:
            total += printer.pprint(x)

        print(total)

class RebindOperator(ActionOperator):
    """ Special Operator to modify the semantic's Operator Cache,
    allowing more concise names of operators
    """

    def __call__(self, target:Union[Sentence, str], op:Union[ProductionOperator, Sentence, str], data=None, semSystem=None):
        """ λUpdateOps "+" $x
        Updates the Sem System's Operator cache
        """
        logging.info("Updating Operator Cache")
        cache = semSystem._operator_cache
        if op in cache:
            operator = cache[op]
        elif isinstance(op, ProductionOperator):
            operator = op
        else:
            raise AcabSemanticException("Bad Target Specified for Operator Rebind", op)
        new_cache = cache.bind_dict({str(target) : operator})
        semSystem._operator_cache = new_cache
