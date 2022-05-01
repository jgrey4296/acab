"""
Definitions of the Core Performance Operators
"""
from __future__ import annotations
import logging as logmod
from enum import Enum
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab import types as AT
from acab import AcabConfig
from acab.core.value.instruction import (ActionOperator,
                                                        ProductionOperator)
from acab.core.util.decorators.semantic import (OperatorArgUnWrap,
                                               OperatorResultWrap,
                                               OperatorSugar)
from acab.error.semantic import AcabSemanticException

logging = logmod.getLogger(__name__)

config = AcabConfig()

Sentence = AT.Sentence
Operator = AT.Operator

# Action operators:

# TODO: add rule modification actions:
# add/remove penumbral condition
# add/remove penumbral action
# add/remove tag?
# insert/remove penumbral action from sequence
# modify hierarchy

# TODO action operators joots
# and/or return instructions for the semantic system
@OperatorSugar("!!")
class AcabAssert(ActionOperator):

    def __call__(self, *params, data=None, semSystem=None):
        """ Assert the params (ie: sentences) """
        # TODO use override?
        # TODO enable queing?
        semSystem(params[0])

@OperatorSugar("%", "action")
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

@OperatorSugar(config.prepare("Parse", "REBIND_SUGAR")())
class RebindOperator(ActionOperator):
    """ Special Operator to modify the semantic's Operator Cache,
    allowing more concise names of operators
    """

    def __call__(self, target:Sentence|str, op:Operator|Sentence|str, *, data=None, semSystem=None):
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
