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

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence
from acab.abstract.data.node import AcabNode
from acab.abstract.data.contexts import Contexts
from acab.abstract.data.structure import DataStructure
from acab.abstract.data.node_semantics import AcabNodeSemantics

from acab.error.acab_semantic_exception import AcabSemanticException

from acab.config import AcabConfig

util = AcabConfig.Get()

OBVIOUS_TYPES                = util.value("Print.Data", "SUPPRESSION_TYPES").split(" ")

# TODO replace this with pulling the dict straight from config
PARAM_JOIN_V            = util.value("Print.Patterns", "PARAM_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])
PRINT_SENTINEL_JOIN_P   = util.prepare("Print.Patterns", "PRINT_SENTINEL_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])
SEN_JOIN_V              = util.value("Print.Patterns", "SEN_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])
TAB_V                   = util.value("Print.Patterns", "TAB", actions=[AcabConfig.actions_e.STRIPQUOTE])
WRAP_FORMAT_V           = util.value("Print.Patterns", "WRAP_FORMAT")
CONTAINER_JOIN_V        = util.value("Print.Patterns", "CONTAINER_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])

AT_BIND_V               = util.value("Value.Structure", "AT_BIND")
BIND_V                  = util.value("Value.Structure", "BIND")
CONSTRAINT_V            = util.value("Value.Structure", "CONSTRAINT")
NEGATION_V              = util.value("Value.Structure", "NEGATION")
OPERATOR_V              = util.value("Value.Structure", "OPERATOR")
QUERY_V                 = util.value("Value.Structure", "QUERY")
TAG_V                   = util.value("Value.Structure", "TAG")
TYPE_INSTANCE_V         = util.value("Value.Structure", "TYPE_INSTANCE")

END_V                   = util.value("Parse.Structure", "END")
FUNC_V                  = util.value("Parse.Structure", "FUNC")

AT_BIND_SYMBOL_V        = util.value("Symbols", "AT_BIND")
BIND_SYMBOL_V           = util.value("Symbols", "BIND")
END_SYMBOL_V            = util.value("Symbols", "END")
FALLBACK_MODAL_SYMBOL_V = util.value("Symbols", "FALLBACK_MODAL", actions=[AcabConfig.actions_e.STRIPQUOTE])
FUNC_SYMBOL_V           = util.value("Symbols", "FUNC")
NEGATION_SYMBOL_V       = util.value("Symbols", "NEGATION")
QUERY_SYMBOL_V          = util.value("Symbols", "QUERY")
TAG_SYMBOL_V            = util.value("Symbols", "TAG")

# pylint: disable       =line-too-long
# AcabValue -> Value(Op), Statement(Sentence, Component) Container(Query, Transform, Action), Structured:(Rule, Agenda, Layer, Pipeline)
Printable               = Union[AcabValue, AcabNode, DataStructure, Contexts, str]
# pylint: enable        =line-too-long

RET_enum = Enum("HandlerEnum", "PASS SIMPLE ACCUMULATOR SUBSTRUCT CALL SENTINEL PRINTABLE")
SEARCH_enum = Enum("Semantic Searches", "UUID NAME VALUE ACAB_T PY_T THROW")

DEFAULT_SEMANTIC_SEARCH = [SEARCH_enum[x] for x in util.value("Print.Data", "DEFAULT_SEMANTIC_SEARCH").split(" ")]

AccumulationDict = Dict[Any, Any]
AccumulatorReturn = Tuple[RET_enum, Dict[str, Any], None]
SubstructReturn = Tuple[RET_enum, List[Printable], Optional['Sentinel']]
SimpleReturn = Tuple[RET_enum, str, None]
HandlerReturnUnion = Union[AccumulatorReturn, SubstructReturn, SimpleReturn]

Handler = Callable[['AcabPrintSemantics', Printable, AccumulationDict, Any], HandlerReturnUnion]
Sentinel = Callable[['AcabPrintSemantics', Printable, List[str], AccumulationDict, Any], HandlerReturnUnion]

SemanticSpec = Tuple[List[Handler], Sentinel]
ContextValue = str
SemBox = Tuple[RET_enum, Printable, Callable]
StackValue = Tuple[List[SemBox], List[str], Dict[Any, Any]]


class AcabPrintSemantics(AcabValue):
    """ Abstract Class of Print Semantics
    Provides the basic walk through of a value/node/container etc
    to call handlers to produce a re-parseable string
    """
    HANDLER_E = RET_enum

    # Utility access for handlers:
    e_print    = RET_enum.PRINTABLE
    e_call     = RET_enum.CALL
    sentinel   = RET_enum.SENTINEL
    accumulate = RET_enum.ACCUMULATOR
    substruct  = RET_enum.SUBSTRUCT
    simple     = RET_enum.SIMPLE
    e_pass     = RET_enum.PASS
    @staticmethod
    def _default_opts(trues, vals):
        """ Create the default options for pprint """
        opts = defaultdict(lambda: False)
        opts.update(default_aliases())
        if trues is not None:
            for x in trues:
                opts[x] = True
        if vals is not None:
            for x, y in vals.items():
                opts[x] = y

        return opts


    @staticmethod
    def default_enum_lookup():
        initial = {x: AcabPrintSemantics._throw_semantic_search for x in SEARCH_enum}
        initial[SEARCH_enum.UUID] = AcabPrintSemantics._get_by_uuid
        initial[SEARCH_enum.NAME] = AcabPrintSemantics._get_by_name
        initial[SEARCH_enum.VALUE] = AcabPrintSemantics._get_by_value
        initial[SEARCH_enum.ACAB_T] = AcabPrintSemantics._get_by_acab_type_hierarchy
        initial[SEARCH_enum.PY_T] = AcabPrintSemantics._get_by_python_type_hierarchy

        return initial

    def __init__(self, type_print_semantics: Dict[Printable, SemanticSpec],
                 default_values: Dict[Any, str] = None,
                 default_true: List[str] = None,
                 search_order: List[Union[Callable, Enum]] = None,
                 enum_lookup_override: Dict[Enum, Callable] = None):

        super(AcabPrintSemantics, self).__init__(None)

        self._uuid_board: Dict['UUID', 'DefaultDict']       = {}
        self._bottom_semantic                               = ([], lambda PS, val, processed, acc, params: (RET_enum.SIMPLE, str(val), None, None))
        self._type_semantics: Dict[Printable, SemanticSpec] = type_print_semantics
        self._opts                                          = AcabPrintSemantics._default_opts(default_true, default_values)
        self._search_order: List[Callable]                  = DEFAULT_SEMANTIC_SEARCH[:]
        self._search_lookup: Dict[Enum, Callable]           = AcabPrintSemantics.default_enum_lookup()
        self._instruction_mapping: Dict[Enum, Callable]     = setup_instruction_mappings()

        assert(len(self._instruction_mapping) == len(RET_enum) - 1)
        # Printing context:
        # Context: Processed strings.
        self._context: List[ContextValue] = []
        # The paused stack of contexts. Invariant: all(x:Tuple for x in stack])
        self._stack: List[StackValue]     = []
        # The current queue of values to process
        self._queue: List[SemBox]         = []
        # Named values to retrieve and use
        self._accumulation: Dict[str, Any]      = {}

        if search_order is not None:
            self._search_order = search_order

        if enum_lookup_override is not None:
            self._search_lookup.update(enum_lookup_override)



    def print(self, values: List[Printable], final_handler: Callable = None, overrides=None) -> str:
        """
        The public print function. Takes a list of values, converts them
        to str's, and combines them using a final-handler or "\n".join
        """
        logging.info("Starting Print on: {}".format(values))
        if not isinstance(values, list):
            values = [values]

        if overrides is not None:
            self.set_overrides(overrides)

        self._context: List[ContextValue]  = []
        self._stack: List[StackValue]      = []
        self._queue: List[SemBox]          = [(RET_enum.PRINTABLE, x, None, None) for x in values]
        self._accumulation: Dict[str, Any] = {}

        while bool(self._stack) or bool(self._queue):
            result_instruction, result, result_sentinel = None, None, None
            if not bool(self._queue):
                instruction_tuple = (RET_enum.SENTINEL, "", lambda ps, s, p, a, params: (RET_enum.SIMPLE, str(p), None, None), None)
            else:
                front = self._queue.pop(0)
                instruction_tuple = front

            assert(len(instruction_tuple) == 4)
            if instruction_tuple[0] is RET_enum.PASS:
                self._pop_stack()
                continue

            instruction_handler = self._instruction_mapping[instruction_tuple[0]]
            logging.info("--------------------")
            logging.info("Running      : {}".format(instruction_tuple))
            logging.info("Queue        : {}".format(len(self._queue)))
            logging.info("Context      : {}".format(len(self._context)))
            logging.info("Accumulation : {}".format(len(self._accumulation)))
            logging.info("Stack        : {}".format(format(len(self._stack))))
            result = instruction_handler(self, instruction_tuple[1], instruction_tuple[2], instruction_tuple[3])
            logging.info("Result       : {}".format(str(result)))
            assert(isinstance(result, tuple))
            # Insert the result at the head of the queue, so its processed next
            self._queue.insert(0, result)



        if final_handler is not None:
            final_val = final_handler(self, self._context, self._accumulation)
            assert(isinstance(final_val, str))
        else:
            # Filter out info tuples if necessary for default:
            default_join = self.use(PRINT_SENTINEL_JOIN_P)
            final_val = default_join.join([x for x in self._context if isinstance(x, str)])


        return final_val



    def ask(self, lookup, for_uuid=None) -> Any:
        """
        Ask the semantics for the value of something
        Will always return at least False
        """
        assert(isinstance(lookup, str))
        if for_uuid is not None and for_uuid in self._uuid_board:
            return self._uuid_board[for_uuid][lookup]

        return self._opts[lookup]


    def use(self, lookup, for_uuid=None):
        """
        Pairs with AcabConfig.prepare, acting as a proxy for Config,
        giving run time overrides backed by Config settings
        """
        assert(isinstance(lookup, tuple))
        assert(len(lookup) == 5)
        # attempt normal lookup
        key = lookup[1]
        assert(isinstance(key, str))
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
        assert(isinstance(value, dict))
        self._accumulation.update(value)

    def _push_stack(self, data, sentinel, params):
        assert(isinstance(data, list))
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
        use the_type (ie: python type) first, if its necessary, differentiate using type_instance

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

    def _get_by_uuid(self, val: Printable) -> SemanticSpec:
        if val._uuid in self._type_semantics:
            return self._type_semantics[val._uuid]

        return None

    def _get_by_name(self, val: Printable) -> Optional[SemanticSpec]:
        if val.name in self._type_semantics:
            return self._type_semantics[val.name]

        return None

    def _get_by_value(self, val: Printable) -> Optional[SemanticSpec]:
        if val.value in self._type_semantics:
            return self._type_semantics[val.name]

        return None


    def _get_by_acab_type_hierarchy(self, val) -> Optional[SemanticSpec]:
        acab_types = [val.type]

        while bool(acab_types):
            current = acab_types.pop(0)
            if current in self._type_semantics:
                return self._type_semantics[current]

        return None

    def _get_by_python_type_hierarchy(self, val) -> Optional[SemanticSpec]:
        types = [val.__class__]
        while bool(types):
            current = types.pop(0)
            if current in self._type_semantics:
                return self._type_semantics[current]

        return None


    def _throw_semantic_search(self, val):
        raise AcabSemanticException(str(val), val)



    def _handle_printable(self, data, func, params):
        # A Printable value to find instructions for
        assert(func is None)
        handlers, sentinel = self._retrieve_semantics(data)
        # Add the handlers to the front of the queue
        result = [(RET_enum.CALL, data, handler, None) for handler in handlers] + [(RET_enum.SENTINEL, data, sentinel, None)]
        self._queue = result + self._queue
        return RET_enum.PASS, None, None, None

    def _handle_simple(self, data, func, params):
        self._add_to_context(data)

        return (RET_enum.PASS, None, None, None)

    def _handle_call(self, data, func, params):
        assert(func is not None)
        return func(self, data, self._accumulation, params)

    def _handle_sentinel(self, data, func, params):
        result_tuple = func(self, data, self._context,
                            self._accumulation, params)
        assert(len(result_tuple) == 4), breakpoint()
        self._pop_stack()
        return result_tuple


    def _handle_accumulator(self, data, sentinel, params):
        assert(isinstance(data, dict))
        self._add_to_accumulation(data)
        return (RET_enum.PASS, None, None, None)
    def _handle_substruct(self, data, sentinel, params):
        assert(isinstance(data, list))
        # PUSH STACK:
        self._push_stack(data, sentinel, params)
        return (RET_enum.PASS, None, None, None)

def default_handler(print_semantics: AcabPrintSemantics, value: Printable, accum, params) -> HandlerReturnUnion:
    """
    The simplest print handler.
    """
    return RET_enum.SIMPLE, str(value), None, None


def default_sentinel(print_semantics: AcabPrintSemantics,
                     value: Printable,
                     ctx: List[ContextValue],
                     accumulator: Dict[Any, Any],
                     params: Any) -> HandlerReturnUnion:
    """
    The Simplest sentinel
    """
    return RET_enum.SIMPLE, " ".join(ctx), None, None

def default_aliases() -> Dict[Any, str]:
    return {AT_BIND_V             : AT_BIND_SYMBOL_V,
            BIND_V                : BIND_SYMBOL_V,
            END_V                 : END_SYMBOL_V,
            FUNC_V                : FUNC_SYMBOL_V,
            NEGATION_V            : NEGATION_SYMBOL_V,
            QUERY_V               : QUERY_SYMBOL_V,
            PRINT_SENTINEL_JOIN_P[1] : util.value(*PRINT_SENTINEL_JOIN_P),
            SEN_JOIN_V            : SEN_JOIN_V,
            TAG_V                 : TAG_SYMBOL_V,
            FALLBACK_MODAL_SYMBOL_V : FALLBACK_MODAL_SYMBOL_V,
            PARAM_JOIN_V          : PARAM_JOIN_V,
            WRAP_FORMAT_V         : WRAP_FORMAT_V
            }

def setup_instruction_mappings() -> Dict[Enum, Callable]:
    return {
        RET_enum.SIMPLE      : AcabPrintSemantics._handle_simple,
        RET_enum.ACCUMULATOR : AcabPrintSemantics._handle_accumulator,
        RET_enum.SUBSTRUCT   : AcabPrintSemantics._handle_substruct,
        RET_enum.CALL        : AcabPrintSemantics._handle_call,
        RET_enum.SENTINEL    : AcabPrintSemantics._handle_sentinel,
        RET_enum.PRINTABLE   : AcabPrintSemantics._handle_printable,
    }
