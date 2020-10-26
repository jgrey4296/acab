#!/usr/bin/env python
""" A Semantics Class for defining how to print out
ACAB structures
"""
import logging as root_logger
logging = root_logger.getLogger(__name__)

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

AT_BIND_S         = util("Parsing.Structure", "AT_BIND_S")
AT_BIND_SYMBOL_S         = util("Visual.Symbols", "AT_BIND_SYMBOL_S")

BIND_S           = util("Parsing.Structure", "BIND_S")
BIND_SYMBOL_S            = util("Visual.Symbols", "BIND_SYMBOL_S")

CONSTRAINT_S     = util("Parsing.Structure", "CONSTRAINT_S")

END_S            = util("Parsing.Structure", "END_S")
END_SYMBOL_S            = util("Visual.Symbols", "END_SYMBOL_S")

FALLBACK_MODAL_S = util("Printing", "FALLBACK_MODAL_S")
FALLBACK_MODAL_SYMBOL_S = util("Visual.Symbols", "FALLBACK_MODAL_SYMBOL_S", action=AcabConfig.actions_e.STRIPQUOTE)

FUNC_S           = util("Parsing.Structure", "FUNC_S")
FUNC_SYMBOL_S           = util("Visual.Symbols", "FUNC_SYMBOL_S")

NEGATION_S       = util("Parsing.Structure", "NEGATION_S")
NEGATION_SYMBOL_S       = util("Visual.Symbols", "NEGATION_SYMBOL_S")

OBVIOUS_TYPES    = []

OPERATOR_S       = util("Parsing.Structure", "OPERATOR_S")

QUERY_S          = util("Parsing.Structure", "QUERY_S")
QUERY_SYMBOL_S          = util("Visual.Symbols", "QUERY_SYMBOL_S")

SEN_JOIN_S       = util("Printing", "SEN_JOIN_S")
SEN_JOIN_SYMBOL_S = util("Visual.Symbols", "SEN_JOIN_SYMBOL_S", action=AcabConfig.actions_e.STRIPQUOTE)

TAB_S            = util("Printing", "TAB_S", action=AcabConfig.actions_e.STRIPQUOTE)

TAG_S            = util("Parsing.Structure", "TAG_S")
TAG_SYMBOL_S            = util("Visual.Symbols", "TAG_SYMBOL_S")

VALUE_TYPE_S     = util("Parsing.Structure", "VALUE_TYPE_S")



# pylint: disable=line-too-long
# AcabValue -> Value(Op, TypeInstance), Statement(Sentence, Component) Container(Query, Transform, Action), Structured:(Rule, Agenda, Layer, Pipeline)
Printable = Union[AcabValue, AcabNode, DataStructure, Contexts, 'TypeInstance', str]
# pylint: enable=line-too-long

RET_enum = Enum("Handler Return Form", "PASS SIMPLE ACCUMULATOR SUBSTRUCT CALL SENTINEL PRINTABLE PUSHSTACK")
SEARCH_enum = Enum("Semantic Searchies", "UUID NAME VALUE ACAB_T PY_T THROW")

DEFAULT_SEMANTIC_SEARCH = [SEARCH_enum[x] for x in util("Printing", "DEFAULT_SEMANTIC_SEARCH").split(" ")]

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

    def __init__(self, type_print_semantics: Dict[Printable, SemanticSpec],
                 default_values: Dict[Any, str] = None,
                 default_true: List[str] = None,
                 search_order: List[Union[Callable, Enum]] = None,
                 enum_lookup_override: Dict[Enum, Callable] = None):

        super(AcabPrintSemantics, self).__init__(None)

        self._uuid_board: Dict['UUID', 'DefaultDict']       = {}
        self._bottom_semantic                               = ([], lambda PS, val, processed, acc, params: (RET_enum.SIMPLE, str(val), None))
        self._type_semantics: Dict[Printable, SemanticSpec] = type_print_semantics
        self._opts                                          = AcabPrintSemantics._default_opts(default_true, default_values)
        self._search_order: List[Callable]                  = DEFAULT_SEMANTIC_SEARCH[:]
        self._search_lookup: Dict[Enum, Callable]           = AcabPrintSemantics.default_enum_lookup()

        if search_order is not None:
            self._search_order = search_order

        if enum_lookup_override is not None:
            self._search_lookup.update(enum_lookup_override)



    def print(self, values: List[Printable], final_handler: Callable = None) -> str:
        """
        The public print function. Takes a list of values, converts them
        to str's, and combines them using a final-handler or "\n".join
        """
        if not isinstance(values, list):
            values = [values]

        # Context: Processed strings.
        context: List[ContextValue] = []
        # The paused stack of contexts. Invariant: all(x:Tuple for x in stack])
        stack: List[StackValue] = []
        # The current queue of values to process
        queue: List[SemBox] = [(RET_enum.PRINTABLE, x, None, None) for x in values]
        # The completed context of strings to join
        processed_sub_ctx: List[ContextValue] = []
        accumulation: Dict[str, Any] = {}

        while bool(stack) or bool(queue):
            result_form, result, result_sentinel = None, None, None
            if not bool(queue):
                instruction, data, func, params = (RET_enum.SENTINEL, "", lambda ps, s, p, a, params: (RET_enum.SIMPLE, str(p), None), None)
            else:
                front = queue.pop(0)
                instruction, data, func, params = front

            if instruction is RET_enum.PRINTABLE:
                # A Printable value to find instructions for
                assert(func is None)
                handlers, sentinel = self.retrieve_semantics(data)
                # Add the handlers to the front of the queue
                result = [(RET_enum.CALL, data, handler, None) for handler in handlers] + [(RET_enum.SENTINEL, data, sentinel, None)]
                result_form = RET_enum.SUBSTRUCT

            elif instruction is RET_enum.SIMPLE:
                # SIMPLE just gets added to the context, nothing to do
                result_form = instruction
                result = data
            elif instruction is RET_enum.CALL:
                result_form, result, result_sentinel = func(self, data, accumulation, params)
            elif instruction is RET_enum.SENTINEL:
                # STACK POP
                # Run a sentinel to collect the context together
                try:
                    result_form, result, result_sentinel = func(self, data, context, accumulation, params)
                except TypeError as err:
                    breakpoint()
                    logging.debug("TypeError")

                context = []
                if not bool(queue):
                    stack_q, stack_ctx, stack_accumulator = stack.pop()
                    queue = stack_q
                    context = stack_ctx
                    accumulation = stack_accumulator

            else:
                raise Exception("Unrecognised Instruction: {}".format(instruction))

            #------------------------------------------------------------
            if result_form is RET_enum.ACCUMULATOR:
                assert(isinstance(result, dict))
                accumulation.update(result)

            elif result_form is RET_enum.SUBSTRUCT:
                assert(isinstance(result, list)), breakpoint()
                # PUSH STACK:
                stack.append((queue, context, accumulation))

                if result_sentinel is not None:
                    result.append((RET_enum.SENTINEL, data, result_sentinel, None))
                queue = result
                context = []
                accumulation = {}

            elif result_form is RET_enum.SIMPLE:
                if isinstance(result, str):
                    context.append(result)
                elif isinstance(result, list):
                    context += result
                else:
                    raise Exception("Expected a str or a list")
            elif result_form is RET_enum.PASS:
                continue
            else:
                raise Exception("Unrecognised Result Form: {} : {}".format(result_form, result))


        if final_handler is not None:
            final_val = final_handler(self, context, accumulation)
            assert(isinstance(final_val, str))
            return final_val
        else:
            # Filter out info tuples if necessary for default:
            # DEFAULT join
            return "\n".join([x for x in context if isinstance(x, str)])




    def ask(self, prefix, suffix=None, for_uuid=None):
        """
        Ask the print interpretation about a setting
        """
        if for_uuid in self._uuid_board:
            return self._uuid_board[for_uuid][prefix]

        return self._opts[prefix]

    def set_for_uuid(self, uuid, default_trues, **kwargs):
        if uuid not in self._uuid_board:
            self._uuid_board[uuid] = self._default_opts(default_trues, kwargs)
        else:
            self._uuid_board[uuid].update(default_trues, kwargs)



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

    def retrieve_semantics(self, current_val: Printable) -> Optional[SemanticSpec]:
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
        raise AcabSemanticException("Semantic Search Failure for: {}".format(str(val)))


def default_handler(print_semantics: AcabPrintSemantics, value: Printable, accum, params) -> HandlerReturnUnion:
    """
    The simplest print handler.
    """
    return RET_enum.SIMPLE, str(value)


def default_sentinel(print_semantics: AcabPrintSemantics,
                     value: Printable,
                     ctx: List[ContextValue],
                     accumulator: Dict[Any, Any],
                     params: Any) -> HandlerReturnUnion:
    """
    The Simplest sentinel
    """
    return RET_enum.SIMPLE, " ".join(ctx)

def default_aliases() -> Dict[Any, str]:
    return {AT_BIND_S : AT_BIND_SYMBOL_S,
            BIND_S : BIND_SYMBOL_S,
            END_S : END_SYMBOL_S,
            FUNC_S : FUNC_SYMBOL_S,
            NEGATION_S : NEGATION_SYMBOL_S,
            QUERY_S : QUERY_SYMBOL_S,
            SEN_JOIN_S : SEN_JOIN_SYMBOL_S,
            TAG_S : TAG_SYMBOL_S,
            FALLBACK_MODAL_S : FALLBACK_MODAL_SYMBOL_S
            }
