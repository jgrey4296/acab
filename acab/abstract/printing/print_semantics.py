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
from acab.abstract.core.contexts import Contexts
from acab.abstract.core.structure import DataStructure
from acab.abstract.core.node_semantics import AcabNodeSemantics

from acab.abstract.interfaces import semantics_interface as SI

from acab.error.acab_semantic_exception import AcabSemanticException

from acab.abstract.config.config import AcabConfig

from . import util as PUtil

util = AcabConfig.Get()

OBVIOUS_TYPES = util.value("Print.Data", "SUPPRESSION_TYPES").split(" ")

# TODO replace this with pulling the dict straight from config
PRINT_SENTINEL_JOIN_P = util.prepare(
    "Print.Patterns", "PRINT_SENTINEL_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE]
)

# pylint: disable       =line-too-long
# AcabValue -> Value(Op), Statement(Sentence, Component) Container(Query, Transform, Action), Structured:(Rule, Agenda, Layer, Pipeline)
Printable = Union[AcabValue, AcabNode, DataStructure, Contexts, str]
# pylint: enable        =line-too-long

RET_enum = PUtil.RET_enum
SEARCH_enum = PUtil.SEARCH_enum

AccumulationDict = Dict[Any, Any]
AccumulatorReturn = Tuple[RET_enum, Dict[str, Any], None]
SubstructReturn = Tuple[RET_enum, List[Printable], Optional["Sentinel"]]
SimpleReturn = Tuple[RET_enum, str, None]
HandlerReturnUnion = Union[AccumulatorReturn, SubstructReturn, SimpleReturn]

Handler = Callable[
    ["AcabPrintSemantics", Printable, AccumulationDict, Any], HandlerReturnUnion
]
Sentinel = Callable[
    ["AcabPrintSemantics", Printable, List[str], AccumulationDict, Any],
    HandlerReturnUnion,
]

SemanticSpec = Tuple[List[Handler], Sentinel]
ContextValue = str
SemBox = Tuple[RET_enum, Printable, Callable]
StackValue = Tuple[List[SemBox], List[str], Dict[Any, Any]]


class AcabPrintSemantics(AcabValue, SI.PrintSemanticInterface):
    """ Abstract Class of Print Semantics
    Provides the basic walk through of a value/node/container etc
    to call handlers to produce a re-parseable string
    """

    HANDLER_E = RET_enum

    # Utility access for handlers:
    e_print = RET_enum.PRINTABLE
    e_call = RET_enum.CALL
    sentinel = RET_enum.SENTINEL
    accumulate = RET_enum.ACCUMULATOR
    substruct = RET_enum.SUBSTRUCT
    simple = RET_enum.SIMPLE
    e_pass = RET_enum.PASS

    @staticmethod
    def _default_opts(trues, vals):
        """ Create the default options for pprint """
        opts = defaultdict(lambda: False)
        opts.update(PUtil.default_aliases())
        if trues is not None:
            for x in trues:
                opts[x] = True
        if vals is not None:
            for x, y in vals.items():
                opts[x] = y

        return opts

    def __init__(
        self,
        type_print_semantics: Dict[Printable, SemanticSpec],
        default_values: Dict[Any, str] = None,
        default_true: List[str] = None,
        search_order: List[Union[Callable, Enum]] = None,
        enum_lookup_override: Dict[Enum, Callable] = None,
    ):

        super(AcabPrintSemantics, self).__init__()

        self._uuid_board: Dict["UUID", "DefaultDict"] = {}
        self._bottom_semantic = (
            [],
            lambda PS, val, processed, acc, params: (
                RET_enum.SIMPLE,
                str(val),
                None,
                None,
            ),
        )
        self._type_semantics: Dict[Printable, SemanticSpec] = type_print_semantics
        self._opts = AcabPrintSemantics._default_opts(default_true, default_values)
        self._search_order: List[Callable] = PUtil.DEFAULT_SEMANTIC_SEARCH[:]
        self._search_lookup: Dict[
            Enum, Callable
        ] = PUtil.default_enum_lookup()
        self._instruction_mapping: Dict[Enum, Callable] = PUtil.setup_instruction_mappings()

        assert len(self._instruction_mapping) == len(RET_enum) - 1
        # Printing context:
        # Context: Processed strings.
        self._context: List[ContextValue] = []
        # The paused stack of contexts. Invariant: all(x:Tuple for x in stack])
        self._stack: List[StackValue] = []
        # The current queue of values to process
        self._queue: List[SemBox] = []
        # Named values to retrieve and use
        self._accumulation: Dict[str, Any] = {}

        if search_order is not None:
            self._search_order = search_order

        if enum_lookup_override is not None:
            self._search_lookup.update(enum_lookup_override)

    def print(
        self, values: List[Printable], final_handler: Callable = None, overrides=None
    ) -> str:
        """
        The public print function. Takes a list of values, converts them
        to str's, and combines them using a final-handler or "\n".join
        """
        logging.info("Starting Print on: {}".format(values))
        if not isinstance(values, list):
            values = [values]

        if overrides is not None:
            self.set_overrides(overrides)

        self._context: List[ContextValue] = []
        self._stack: List[StackValue] = []
        self._queue: List[SemBox] = [
            (RET_enum.PRINTABLE, x, None, None) for x in values
        ]
        self._accumulation: Dict[str, Any] = {}

        while bool(self._stack) or bool(self._queue):
            result_instruction, result, result_sentinel = None, None, None
            if not bool(self._queue):
                instruction_tuple = (
                    RET_enum.SENTINEL,
                    "",
                    lambda ps, s, p, a, params: (RET_enum.SIMPLE, str(p), None, None),
                    None,
                )
            else:
                front = self._queue.pop(0)
                instruction_tuple = front

            assert len(instruction_tuple) == 4
            if instruction_tuple[0] is RET_enum.PASS:
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

    def ask(self, lookup, for_uuid=None) -> Any:
        """
        Ask the semantics for the value of something
        Will always return at least False
        """
        if for_uuid is not None and for_uuid in self._uuid_board:
            return self._uuid_board[for_uuid][lookup]

        return self._opts[lookup]

    def use(self, lookup, for_uuid=None):
        """
        Pairs with AcabConfig.prepare, acting as a proxy for Config,
        giving run time overrides backed by Config settings
        """
        assert isinstance(lookup, tuple)
        assert len(lookup) == 5
        # attempt normal lookup
        key = lookup[1]
        assert isinstance(key, str)
        result = self.ask(key, for_uuid=for_uuid)

        if result:
            return result

        # then config lookup
        return util.value(*lookup)

    def set_for_uuid(self, uuid, default_trues, **kwargs):
        if uuid not in self._uuid_board:
            self._uuid_board[uuid] = self._default_opts(default_trues, kwargs)
        else:
            self._uuid_board[uuid].update(default_trues, kwargs)

    def set_overrides(self, overrides):
        self._opts.update(overrides)

    def _add_to_context(self, value):
        if isinstance(value, str):
            self._context.append(value)
        elif isinstance(value, list):
            self._context += value
        else:
            raise Exception("Expected a str or a list")

    def _add_to_accumulation(self, value):
        assert isinstance(value, dict)
        self._accumulation.update(value)

    def _push_stack(self, data, sentinel, params):
        assert isinstance(data, list)
        self._stack.append((self._queue, self._context))

        if sentinel is not None:
            data.append((RET_enum.SENTINEL, data, sentinel, params))

        self._queue = data
        self._context = []

    def _pop_stack(self):
        if not bool(self._queue) and bool(self._stack):
            stack_q, stack_ctx = self._stack.pop()
            self._queue = stack_q
            self._context = stack_ctx

    def _retrieve_semantics(self, current_val: Printable) -> Optional[SemanticSpec]:
        """
        use the_type (ie: python type) first, if its necessary, distinguish using type_instance

        Always returns, even if its just lambda x: str(x)
        """
        chosen: Callable = self._bottom_semantic
        # search_order: List[Callable[[AcabPrintSemantics, Printable], Optional[SemanticSpec]]] = []
        search_order = self._search_order[:]

        for x in search_order:
            search_f = x
            if x in self._search_lookup:
                search_f = self._search_lookup[x]

            result = search_f(self, current_val)
            if result is not None:
                chosen = result
                break

        # TODO update _type_semantics chain with found bindings from hierarchy

        return chosen


