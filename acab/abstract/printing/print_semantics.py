#!/usr/bin/env python
""" A Semantics Class for defining how to print out
ACAB structures
"""
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar

from collections import defaultdict

from acab.abstract.core.type_base import TypeInstance
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
# AcabValue -> Value, Statement, Sentence, Op, Component, Container, Query, Transform, Action, Rule, Agenda, Layer, Pipeline
Printable = Union[AcabValue, AcabNode, DataStructure, Contexts, TypeInstance, str]
# pylint: enable=line-too-long

HandlerContinuation h Union[str, Tuple[Printable, List[Printable], Callable]]

ContextValues = Union[str, Tuple[Any]]
StackValues = Tuple[List[Printable],
                    List[ContextValues]]
QueueValues = Union[Printable,
                    Tuple[Printable, Callable]]



class AcabPrintSemantics:
    """ Abstract Class of Print Semantics
    Provides the basic walk through of a value/node/container etc
    to call handlers to produce a re-parseable string
    """

    def __init__(self,
                 type_print_semantics: Dict[AcabValue, Callable],
                 str_aliases: Dict[Any, str],
                 default_true: List[str] = None,
                 default_values: Dict[str, Any] = None):

        self._uuid_board: Dict['UUID', 'DefaultDict'] = {}
        self._context_stack: List[List[ContextValues]] = []
        self._current_context: List[ContextValues] = []
        self._bottom_semantic = lambda s, x: str(x)
        self._type_semantics = type_print_semantics
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
        for x in trues:
            opts[x] = True
        for x, y in vals.items():
            opts[x] = y
        return opts

    def retrieve_semantics(self, val: Printable) -> Union[Callable, str]:
        """
        use the_type (ie: python type) first, if its necessary, differentiate using type_instance

        Always returns, even if its just lambda x: str(x)
        """
        chosen: Callable = self._bottom_semantic

        # first do a straight lookup:
        if val in self._type_semantics:
            return self._type_semantics[val]

        # if a str, check aliases
        if isinstance(val, str) and val in self._aliases:
            return self._aliases[val]

        # Next lookup by UUID
        val_uuid = val._uuid
        if val_uuid in self._type_semantics:
            return self._type_semantics[val_uuid]

        # Then try the acab type hierarchy

        # then try the python type hierarchy

        # update _type_semantics chain with found bindings from hierarchy


        # then error


    def old_retrieve_semantics(self, val: Printable) -> Callable:
        # TODO : override tuples
        # TODO should I be using my type instances for semantics?
        curr = type_instance
        retrieved = None
        descendents_to_update = []
        # TODO adjust the climb:
        while retrieved is None and curr not in (object, None):
            if curr in self._type_semantics:
                retrieved = self._type_semantics[curr]
            else:
                curr = curr.__base__
                descendents_to_update.append(curr)

        if retrieved is None:
            # TODO: should this be a warning?
            msg = "Missing Print Semantic Binding for: {}".format(the_type)
            raise AcabSemanticException(msg, the_type)

        if len(descendents_to_update) > 1:
            # Cache the climb
            self._type_semantics.update({x : retrieved for x in descendents_to_update})

        return retrieved

    def print(self, values: List[Printable], final_handler: Callable = None) -> str:
        """
        The public print function. Takes a list of values, converts them
        to str's, and combines them using a final-handler or "\n".join
        """
        # Context: Processed strings.
        context: List[ContextValues] = []
        # The paused stack of contexts. Invariant: all(x:Tuple for x in stack])
        stack: List[StackValues] = []
        # The current queue of values to process
        queue: List[QueueValues] = values[:]
        # The completed context of strings to join
        processed_sub_ctx: Optional[List[ContextValues]] = None

        while bool(stack) or bool(queue):
            result = None
            direct_add = False
            if not bool(queue):
                # Queue is empty, so get the next paused context
                processed_sub_ctx = context
                stack_q, stack_ctx = stack.pop()
                context = stack_ctx
                queue = stack_q

            # Get something to process
            val = queue.pop(0)
            if processed_sub_ctx is not None and isinstance(val, tuple):
                # got a completed context and a handler for it, use it
                assert(all([isinstance(x, (tuple, str)) for x in processed_sub_ctx]))
                source, finaliser = val
                result = finaliser(self, source, processed_sub_ctx)
                assert(isinstance(result, str))
                direct_add = True
                processed_sub_ctx = None

            elif isinstance(val, (str, tuple)):
                # Ignore tuples, they are info for handlers
                # Ignore strings because theres nothing to do to them
                result = val
                direct_add = True
            else:
                # Otherwiser, val is a printable, so start the process
                handler = self.retrieve_semantics(val)
                result = handler(self, val)


            assert(result is not None)
            if direct_add or isinstance(result, str):
                # Handler returned a non-continued clause : You're done
                context.append(result)
            else:
                # Handler returned incomplete
                # So pause current context, step down, process, step up
                source, remaining, finaliser = result
                queue.insert(0, (source, finaliser))
                stack.append((queue, context))
                queue = remaining
                context = []


        # TODO : finalise the final context with a handler if necessary
        if final_handler is not None:
            # handler is responsible for filtering out tuples:
            return final_handler(self, context)
        else:
            # Filter out info tuples if necessary for default:
            # DEFAULT join
            return "\n".join([x for x in context if isinstance(x, str)])



def default_handler(print_semantics: AcabPrintSemantics, value: Printable) -> HandlerContinuation:
    """
    The simplest print handler.
    """
    return str(value)


def default_finaliser(print_semantics: AcabPrintSemantics,
                      value: Printable, ctx: List[Union[Tuple[Any], str]]) -> str:
    """
    The Simplest finaliser
    """
    return " ".join(ctx)
