"""
Utilities for printing out information
All deprecated now in favour of
print_semantics.py
wrappers.py
default_handlers.py
"""
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from re import Pattern

from collections import defaultdict
from enum import Enum

from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.core.node import AcabNode
from acab.abstract.core.struct import AcabStruct

from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import AcabValue
from acab.abstract.core.node import AcabNode


config        = AcabConfig.Get()
OBVIOUS_TYPES = config.value("Print.Data", "SUPPRESSION_TYPES").split(" ")


# TODO replace this with pulling the dict straight from config
PARAM_JOIN_V                                         = config.value(
    "Print.Patterns", "PARAM_JOIN", actions          =[AcabConfig.actions_e.STRIPQUOTE]
)
PRINT_SENTINEL_JOIN_P                                = config.prepare(
    "Print.Patterns", "PRINT_SENTINEL_JOIN", actions =[AcabConfig.actions_e.STRIPQUOTE]
)
SEN_JOIN_V                                           = config.value(
    "Print.Patterns", "SEN_JOIN", actions            =[AcabConfig.actions_e.STRIPQUOTE]
)
TAB_V                                                = config.value("Print.Patterns", "TAB", actions=[AcabConfig.actions_e.STRIPQUOTE])
WRAP_FORMAT_V                                        = config.value("Print.Patterns", "WRAP_FORMAT")
CONTAINER_JOIN_V                                     = config.value(
    "Print.Patterns", "CONTAINER_JOIN", actions      =[AcabConfig.actions_e.STRIPQUOTE]
)

AT_BIND_V                                = config.value("Value.Structure", "AT_BIND")
BIND_V                                   = config.value("Value.Structure", "BIND")
CONSTRAINT_V                             = config.value("Value.Structure", "CONSTRAINT")
NEGATION_V                               = config.value("Value.Structure", "NEGATION")
OPERATOR_V                               = config.value("Value.Structure", "OPERATOR")
QUERY_V                                  = config.value("Value.Structure", "QUERY")
TAG_V                                    = config.value("Value.Structure", "TAG")
TYPE_INSTANCE_V                          = config.value("Value.Structure", "TYPE_INSTANCE")

END_V                                    = config.value("Parse.Structure", "END")
FUNC_V                                   = config.value("Parse.Structure", "FUNC")

AT_BIND_SYMBOL_V                         = config.value("Symbols", "AT_BIND")
BIND_SYMBOL_V                            = config.value("Symbols", "BIND")
END_SYMBOL_V                             = config.value("Symbols", "END")
FALLBACK_MODAL_SYMBOL_V                  = config.value(
    "Symbols", "FALLBACK_MODAL", actions =[AcabConfig.actions_e.STRIPQUOTE]
)
FUNC_SYMBOL_V                            = config.value("Symbols", "FUNC")
NEGATION_SYMBOL_V                        = config.value("Symbols", "NEGATION")
QUERY_SYMBOL_V                           = config.value("Symbols", "QUERY")
TAG_SYMBOL_V                             = config.value("Symbols", "TAG")


def _handle_printable(printer, data, func, params):
        # A Printable value to find instructions for
        assert func is None
        handlers, sentinel = printer._retrieve_semantics(data)
        # Add the handlers to the front of the queue
        result = [(RET_enum.CALL, data, handler, None) for handler in handlers] + [
            (RET_enum.SENTINEL, data, sentinel, None)
        ]
        printer._queue = result + printer._queue
        return RET_enum.PASS, None, None, None

def _handle_simple(printer, data, func, params):
        printer._add_to_context(data)

        return (RET_enum.PASS, None, None, None)

def _handle_call(printer, data, func, params):
        assert func is not None
        return func(printer, data, printer._accumulation, params)

def _handle_sentinel(printer, data, func, params):
        result_tuple = func(printer, data, printer._context, printer._accumulation, params)
        assert len(result_tuple) == 4, breakpoint()
        printer._pop_stack()
        return result_tuple

def _handle_accumulator(printer, data, sentinel, params):
        assert isinstance(data, dict)
        printer._add_to_accumulation(data)
        return (RET_enum.PASS, None, None, None)

def _handle_substruct(printer, data, sentinel, params):
        assert isinstance(data, list)
        # PUSH STACK:
        printer._push_stack(data, sentinel, params)
        return (RET_enum.PASS, None, None, None)





def default_handler(print_semantics: 'AcabPrintSemantics', value: 'Printable', accum, params) -> HandlerReturnUnion:
    """
    The simplest print handler.
    """
    return RET_enum.SIMPLE, str(value), None, None


def default_sentinel(print_semantics: 'AcabPrintSemantics',
                     value: Printable,
                     ctx: List[ContextValue],
                     accumulator: Dict[Any, Any],
                     params: Any,
                     ) -> HandlerReturnUnion:
    """
    The Simplest sentinel
    """
    return RET_enum.SIMPLE, " ".join(ctx), None, None


def default_aliases() -> Dict[Any, str]:
    return {
        AT_BIND_V: AT_BIND_SYMBOL_V,
        BIND_V: BIND_SYMBOL_V,
        END_V: END_SYMBOL_V,
        FUNC_V: FUNC_SYMBOL_V,
        NEGATION_V: NEGATION_SYMBOL_V,
        QUERY_V: QUERY_SYMBOL_V,
        PRINT_SENTINEL_JOIN_P[1]: config.value(*PRINT_SENTINEL_JOIN_P),
        SEN_JOIN_V: SEN_JOIN_V,
        TAG_V: TAG_SYMBOL_V,
        FALLBACK_MODAL_SYMBOL_V: FALLBACK_MODAL_SYMBOL_V,
        PARAM_JOIN_V: PARAM_JOIN_V,
        WRAP_FORMAT_V: WRAP_FORMAT_V,
    }



def setup_instruction_mappings() -> Dict[Enum, Callable]:
    return {
        RET_enum.SIMPLE      : _handle_simple,
        RET_enum.ACCUMULATOR : _handle_accumulator,
        RET_enum.SUBSTRUCT   : _handle_substruct,
        RET_enum.CALL        : _handle_call,
        RET_enum.SENTINEL    : _handle_sentinel,
        RET_enum.PRINTABLE   : _handle_printable,
    }
