#!/usr/bin/env python3
"""
A DSL interface for the system, which

"""
import logging as logmod
import traceback
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.core.parsing.debug_funcs as DBF
import pyparsing as pp

logging = logmod.getLogger(__name__)

import acab.interfaces.dsl as dsl
from acab import GET
from acab import types as AT
from acab.core.util.decorators.dsl import EnsureDSLInitialised
from acab.core.parsing import dsl_builder as DSLImpl
from acab.core.parsing.funcs import clear_parser_names, deep_update_names
from acab.core.util.part_implementations import handler_system as HS
from acab.error.base import AcabBasicException
from acab.error.parse import AcabParseException
from acab.interfaces.dsl import DSL_Builder_i, DSL_Fragment_i, DSL_Spec_i

config = GET()
DEFAULT_HANDLER_SIGNAL = config.prepare("Handler.System", "DEFAULT_SIGNAL")()

Parser           = "pp.ParserElement"
Sentence         = AT.Sentence
Query            = AT.Container
ModuleComponents = AT.ModuleComponents
PyParse_Spec     = "PyParse_Spec"
File             = 'FileObj'

DSL_Fragment = DSLImpl.DSL_Fragment
#----------------------------------------
@dataclass
class PyParse_Handler(HS.Handler, dsl.DSL_Handler_i):
    """ Register a function for handling a DSL setup signal.
    ie: This function is run to set a pyparsing `Forward`

    This doesn't need to use a PatchHandler, because __call__'s are
    only for debugging, DSL's build using the func directly
    """

    func : Parser = field(kw_only=True)

    def __post_init__(self):
        if not isinstance(self.func, pp.ParserElement):
            raise AcabParseException(f"Handler for `{self.signal}` not given a parser")

    def __repr__(self):
        func_expr = None
        if hasattr(self.func, 'expr'):
            func_expr = self.func.expr
        elif hasattr(self.func, 'exprs'):
            func_expr = self.func.exprs

        return f"DSL_Handler({self.signal}, {self.func}, {func_expr})"

    def __call__(self, the_str):
        """ A DSL Handler parses an input string when called """
        return self.func.parse_string(the_str)[:]

    def verify(self, instruction) -> bool:
        return isinstance(instruction, str)


@dataclass
class PyParse_Spec(DSLImpl.DSL_Spec, dsl.DSL_Spec_i):
    """
    Register a signal into a DSL,

    """
    struct : set[Parser] = field(default_factory=set)
    debug  : bool        = field(default=False)

    def __post_init__(self):
        if self.signal == DEFAULT_HANDLER_SIGNAL:
            return
        if self.struct is None:
            raise AcabParseException(f"Signal `{self.signal}` lacks a target")
        if not isinstance(self.struct, set):
            self.struct = set([self.struct])
        if any([not isinstance(x, pp.Forward) for x in self.struct]):
            raise AcabParseException(f"Signal `{self.signal}` isn't paired to a `Forward` Parser")

    def extend_spec(self, spec:PyParse_Spec):
        if not isinstance(spec, PyParse_Spec):
            raise AcabParseException(f"Tried to extend a PyParse_Spec with {spec}")

        self.struct.update(spec.struct)

    def register(self, handler: dsl.DSL_Handler_i):
        assert(isinstance(handler, dsl.DSL_Handler_i))
        if handler.func is None:
            raise AcabParseException(f"`{self.signal}` attempted to register a handler without a parser")

        # TODO flag logic, api's
        self.handlers.append(handler)

    def __call__(self, *args, **kwargs):
        """
        DSL spec call logic is different from handler system
        """
        return self.build()

    def parse_string(self, *args, **kwargs):
        combined = self.build()
        return combined.parse_string(*args, **kwargs)

    def build(self):
        #assert(self.flag_e.COLLECT in self.flags)
        results = [x.func for x in self.handlers if x.func is not None]
        output  = pp.Empty()

        # TODO improve logic with flags
        #
        if not bool(results):
            logging.debug("No Parsers provided for signal: `{}`", self.signal)
            output = pp.NoMatch()
        elif len(results) == 1:
            output = results[0]
        else:
            output = pp.MatchFirst(results)

        for struct in self.struct:
            struct << output
            # At this point, parser is constructed, and will not change again
            # however, *can not* deep-copy the parser for multiple versions.
            #
            # Then for next use of parsers to generate their own name again
            clear_parser_names(struct)
            # Propagate parser names, even through Forwards
            deep_update_names(struct)

        return output

    def setDebug(self, *, flag=None) -> bool:
        if not DBF.debug_pyparsing_active_p():
            logging.warning("PyParsing Debug is not Active")
            return False

        if flag is None:
            flag = not self.debug

        self.debug = flag
        logging.warning(f"Debug <{self.signal}>: {self.debug}")
        for parser in self.handlers:
            parser.func.setDebug(self.debug)

        return self.debug

#----------------------------------------
class PyParseDSL(DSLImpl.DSL_Builder, dsl.DSL_Builder_i):

    def _register_default(self):
        if DEFAULT_HANDLER_SIGNAL in self and bool(self.handler_specs[DEFAULT_HANDLER_SIGNAL]):
            return

        self._register_spec(PyParse_Spec(DEFAULT_HANDLER_SIGNAL))

    @EnsureDSLInitialised
    def parse(self, s:str) -> list[Sentence]:
        if not bool(self[DEFAULT_HANDLER_SIGNAL]):
            raise AcabParseException("No Default Parser Set for DSL")

        try:
            return self[DEFAULT_HANDLER_SIGNAL].parse_string(s, parseAll=True)[:]
        except pp.ParseException as exp:
            logging.warning("\n--------------------\nParse Failure:\n")
            traceback.print_tb(exp.__traceback__)
            logging.warning(f"\n{exp.args[-1]}\n")
            logging.warning(f"Parser: {exp.parser_element}\n")
            logging.warning("Line: {}, Col: {} : {}".format(exp.lineno, exp.col, exp.markInputline()))
            return []


    def debug_parser(self, s:str):
        if s in self:
            parser = self.handler_specs[s]
            return parser.setDebug()

    def disable_debugs(self):
        for spec in self.handler_specs.values():
            spec.setDebug(False)

    def active_debugs(self) -> list[str]:
        result = []
        for signal, spec in self.handler_specs.items():
            if spec.debug:
                result.append(signal)

        return result
