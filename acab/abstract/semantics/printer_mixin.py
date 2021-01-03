#!/usr/bin/env python
""" A Semantics Class for defining how to print out
ACAB structures
"""
import logging as root_logger

logging = root_logger.getLogger(__name__).root

from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar

from collections import defaultdict
from enum import Enum

from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.core.node import AcabNode
from acab.abstract.core.acab_struct import AcabStruct

from acab.abstract.interfaces import semantic_interfaces as SI

from acab.error.acab_semantic_exception import AcabSemanticException

from acab.abstract.config.config import AcabConfig

from . import util as SemUtil

config = AcabConfig.Get()

OBVIOUS_TYPES = config.value("Print.Data", "SUPPRESSION_TYPES").split(" ")

# TODO replace this with pulling the dict straight from config
PRINT_SENTINEL_JOIN_P = config.prepare(
    "Print.Patterns", "PRINT_SENTINEL_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE]
)

# TODO make a cleanup handler *as well*?
class PrinterMixin(SI.SemanticMixin):
    """ Abstract Class of Print Semantics
    Provides the basic walk through of a value/node/container etc
    to call handlers to produce a re-parseable string
    """
    # TODO add expectations of cleanup and message semantics

    def print(self, values: List[SemUtil.Printable]):
        """
        The public print function. Takes a list of values, converts them
        to str's, and combines them using a final-handler or "\n".join
        """
        logging.info("Starting Print on: {}".format(values))
        if not isinstance(values, list):
            values = [values]

        if overrides is not None:
            self.set_overrides(overrides)

        self._context: List[SemUtil.ContextValue] = []
        self._stack: List[SemUtil.StackValue] = []
        self._queue: List[SemUtil.SemBox] = [
            (SemUtil.RET_enum.PRINTABLE, x, None, None) for x in values
        ]
        self._accumulation: Dict[str, Any] = {}

        while bool(self._stack) or bool(self._queue):
            result_instruction, result, result_sentinel = None, None, None
            if not bool(self._queue):
                instruction_tuple = (
                    SemUtil.RET_enum.SENTINEL,
                    "",
                    lambda ps, s, p, a, params: (SemUtil.RET_enum.SIMPLE, str(p), None, None),
                    None,
                )
            else:
                front = self._queue.pop(0)
                instruction_tuple = front

            assert len(instruction_tuple) == 4
            if instruction_tuple[0] is SemUtil.RET_enum.PASS:
                self._pop_stack()
                continue

            instruction_handler = self._instruction_mapping[instruction_tuple[0]]
            logging.info("--------------------")
            logging.info("Running      : {}".format(instruction_tuple))
            logging.info("Queue        : {}".format(len(self._queue)))
            logging.info("Context      : {}".format(" ".join(self._context)))
            logging.info("Accumulation : {}".format(str(self._accumulation)))
            logging.info("Stack        : {}".format(format(len(self._stack))))
            result = instruction_handler(
                self, instruction_tuple[1], instruction_tuple[2], instruction_tuple[3]
            )
            logging.info("Result       : {}".format(str(result)))
            assert isinstance(result, tuple)
            # Insert the result at the head of the queue, so its processed next
            self._queue.insert(0, result)

        return

    def finally(self):
        if final_handler is not None:
            final_val = final_handler(self, self._context, self._accumulation)
            assert isinstance(final_val, str)
        else:
            # Filter out info tuples if necessary for default:
            default_join = self.use(PRINT_SENTINEL_JOIN_P)
            final_val = default_join.join(
                [x for x in self._context if isinstance(x, str)]
            )

        return final_val
