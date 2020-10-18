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

# pylint: disable=bad-whitespace
CONSTRAINT_S      = util("Parsing.Structure", "CONSTRAINT_S")
OPERATOR_S        = util("Parsing.Structure", "OPERATOR_S")
QUERY_S           = util("Parsing.Structure", "QUERY_S")
NEGATION_S        = util("Parsing.Structure", "NEGATION_S")
FALLBACK_S        = util("Parsing.Structure", "FALLBACK_S")
VALUE_TYPE_S      = util("Parsing.Structure", "VALUE_TYPE_S")

FUNC_SYMBOL_S     = util("Printing", "FUNC_SYMBOL_S")

VAR_SYMBOL_S      = util("Parsing", "VAR_SYMBOL_S")
AT_VAR_SYMBOL_S   = util("Parsing", "AT_VAR_SYMBOL_S")
QUERY_SYMBOL_S    = util("Parsing", "QUERY_SYMBOL_S")
NEGATION_SYMBOL_S = util("Parsing", "NEGATION_SYMBOL_S")
TAG_SYMBOL_S      = util("Parsing", "TAG_SYMBOL_S")
END_S             = util("Parsing", "END_S")

TAB_S             = util("Printing", "TAB_S", action=AcabConfig.actions_e.STRIPQUOTE)
FALLBACK_MODAL_S  = util("Printing", "FALLBACK_MODAL_S", action=AcabConfig.actions_e.STRIPQUOTE)
# pylint: enable=bad-whitespace

# pylint: disable=line-too-long
# AcabValue -> Value(Op, TypeInstance), Statement(Sentence, Component) Container(Query, Transform, Action), Structured:(Rule, Agenda, Layer, Pipeline)
Printable = Union[AcabValue, AcabNode, DataStructure, Contexts, 'TypeInstance', str]
# pylint: enable=line-too-long

RET_enum = Enum("Handler Return Form", "PASS SIMPLE ACCUMULATOR SUBSTRUCT CALL SENTINEL PRINTABLE PUSHSTACK")

AccumulatorReturn = Tuple[RET_enum, Dict[str, Any], None]
SubstructReturn = Tuple[RET_enum, List[Printable], Optional['Sentinel']]
SimpleReturn = Tuple[RET_enum, str, None]
HandlerReturnUnion = Union[AccumulatorReturn, SubstructReturn, SimpleReturn]

Handler = Callable[['AcabPrintSemantics', Printable], HandlerReturnUnion]
Sentinel = Callable[['AcabPrintSemantics', Printable, List[str], Dict[Any, Any]], HandlerReturnUnion]

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
    e_print = RET_enum.PRINTABLE
    e_call = RET_enum.CALL
    accumulate = RET_enum.ACCUMULATOR
    substruct = RET_enum.SUBSTRUCT
    simple = RET_enum.SIMPLE
    e_pass = RET_enum.PASS

    def __init__(self,
                 type_print_semantics: Dict[Printable, SemanticSpec],
                 str_aliases: Dict[Any, str],
                 default_true: List[str] = None,
                 default_values: Dict[str, Any] = None):
        super(AcabPrintSemantics, self).__init__(None)

        self._uuid_board: Dict['UUID', 'DefaultDict'] = {}
        self._bottom_semantic = ([], lambda PS, val, processed, acc: (RET_enum.SIMPLE, str(val), None))
        self._type_semantics: Dict[Printable, SemanticSpec] = type_print_semantics
        self._aliases = str_aliases
        self._opts = self.default_opts(default_true, default_values)


    def get_alias(self, value) -> str:
        """ Get an alias in this print interpretation.
        IE: modal enums: OP -> "." and EX -> "!"
        """
        assert(value in self._aliases)
        return self._aliases[value]

    def ask(self, prefix, suffix=None, for_uuid=None):
        """
        Ask the print interpretation about a setting
        """
        # TODO suffix
        if for_uuid in self._uuid_board:
            return self._uuid_board[for_uuid][prefix]

        return self._opts[prefix]

    def set_for_uuid(self, uuid, default_trues, **kwargs):
        if uuid not in self._uuid_board:
            self._uuid_board[uuid] = self.default_opts(default_trues, kwargs)
        else:
            self._uuid_board[uuid].update(default_trues, kwargs)



    @staticmethod
    def default_opts(trues, vals):
        """ Create the default options for pprint """
        opts = defaultdict(lambda: False)
        if trues is not None:
            for x in trues:
                opts[x] = True
        if vals is not None:
            for x, y in vals.items():
                opts[x] = y
        return opts

    def retrieve_semantics(self, current_val: Printable) -> Union[SemanticSpec, str]:
        """
        use the_type (ie: python type) first, if its necessary, differentiate using type_instance

        Always returns, even if its just lambda x: str(x)
        """
        chosen: Callable = self._bottom_semantic

        # first do a straight lookup:
        if current_val in self._type_semantics:
            return self._type_semantics[current_val]

        # if a str, check aliases
        if isinstance(current_val, str) and current_val in self._aliases:
            return self._aliases[current_val]

        # Next lookup by UUID
        val_uuid = current_val._uuid
        if val_uuid in self._type_semantics:
            return self._type_semantics[val_uuid]

        # TODO Then try the acab type hierarchy

        # TODO then try the python type hierarchy
        if current_val.__class__ in self._type_semantics:
            return self._type_semantics[current_val.__class__]

        # TODO update _type_semantics chain with found bindings from hierarchy


        return chosen


    def old_retrieve_semantics(self, current_val: Printable) -> Callable:
        retrieved = None
        descendents_to_update = []
        # TODO adjust the climb:
        # while retrieved is None and curr not in (object, None):
        #     if curr in self._type_semantics:
        #         retrieved = self._type_semantics[curr]
        #     else:
        #         curr = curr.__base__
        #         descendents_to_update.append(curr)

        # if retrieved is None:
        #     # TODO: should this be a warning?
        #     msg = "Missing Print Semantic Binding for: {}".format(the_type)
        #     raise AcabSemanticException(msg, the_type)

        # if len(descendents_to_update) > 1:
        #     # Cache the climb
        #     self._type_semantics.update({x : retrieved for x in descendents_to_update})

        return None

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
        queue: List[SemBox] = [(RET_enum.PRINTABLE, x, None) for x in values]
        # The completed context of strings to join
        processed_sub_ctx: List[ContextValue] = []
        accumulator: Dict[str, Any] = {}
        
        while bool(stack) or bool(queue):
            result_form, result, result_sentinel = None, None, None
            if not bool(queue):
                instruction, data, func = (RET_enum.SENTINEL, "", lambda ps, s, p, a: (RET_enum.SIMPLE, str(p), None))
            else:
                instruction, data, func = queue.pop(0)


            if instruction is RET_enum.PRINTABLE:
                # A Printable value to find instructions for
                assert(func is None)
                handlers, sentinel = self.retrieve_semantics(data)
                # Add the handlers to the front of the queue
                result = [(RET_enum.CALL, data, handler) for handler in handlers] + [(RET_enum.SENTINEL, data, sentinel)]
                result_form = RET_enum.SUBSTRUCT

            elif instruction is RET_enum.SIMPLE:
                # SIMPLE just gets added to the context, nothing to do
                result_form = instruction
                result = data
            elif instruction is RET_enum.CALL:
                result_form, result, result_sentinel = func(self, data, accumulator)
            elif instruction is RET_enum.SENTINEL:
                # STACK POP
                # Run a sentinel to collect the context together
                result_form, result, result_sentinel = func(self, data, context, accumulator)
                context = []
                if not bool(queue):
                    stack_q, stack_ctx, stack_accumulator = stack.pop()
                    queue = stack_q
                    context = stack_ctx
                    accumulator = stack_accumulator

            else:
                raise Exception("Unrecognised Instruction: {}".format(instruction))

            #------------------------------------------------------------
            if result_form is RET_enum.ACCUMULATOR:
                assert(isinstance(result, dict))
                accumulator.update(result)

            elif result_form is RET_enum.SUBSTRUCT:
                assert(all([isinstance(x, tuple) for x in result]))
                # PUSH STACK:
                stack.append((queue, context, accumulator))

                if result_sentinel is not None:
                    result.append((RET_enum.SENTINEL, data, result_sentinel))
                queue = result
                context = []
                accumulator = {}

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
            final_val = final_handler(self, context, accumulator)
            assert(isinstance(final_val, str))
            return final_val
        else:
            # Filter out info tuples if necessary for default:
            # DEFAULT join
            return "\n".join([x for x in context if isinstance(x, str)])



def default_handler(print_semantics: AcabPrintSemantics, value: Printable) -> HandlerReturnUnion:
    """
    The simplest print handler.
    """
    return RET_enum.SIMPLE, str(value)


def default_sentinel(print_semantics: AcabPrintSemantics,
                     value: Printable,
                     ctx: List[ContextValue],
                     accumulator: Dict[Any, Any]) -> HandlerReturnUnion:
    """
    The Simplest sentinel
    """
    return RET_enum.SIMPLE, " ".join(ctx)
