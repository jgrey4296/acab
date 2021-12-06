#!/usr/bin/env python3
"""
A DSL interface for the system, which

"""
import logging as root_logger
import traceback
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import pyparsing as pp

logging = root_logger.getLogger(__name__)

from acab import types as AT
from acab.core.decorators.dsl import EnsureDSLInitialised
from acab.error.acab_exception import AcabBasicException
from acab.error.parse_exception import AcabParseException
from acab.core.parsing.funcs import deep_update_names, clear_parser_names
from acab.interfaces.dsl import DSL_Handler_i, DSL_Fragment, DSL_Spec_i, DSL_Builder_i

Parser           = "pp.ParserElement"
Sentence         = AT.Sentence
Query            = AT.Container
ModuleComponents = AT.ModuleComponents
PyParse_Spec         = "PyParse_Spec"
File             = 'FileObj'

#----------------------------------------
@dataclass
class PyParse_Handler(DSL_Handler_i):
    """ Register a function for handling a DSL setup signal.
    ie: This function is run to set a pyparsing `Forward`"""

    func : Parser = field()

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
        return self.func.parseString(the_str)[:]

@dataclass
class PyParse_Spec(DSL_Spec_i):
    """
    Register a signal into a DSL,

    """
    struct : Set[Parser] = field(default_factory=set)

    def __post_init__(self):
        if self.signal == "_:_default":
            return
        if self.struct is None:
            raise AcabParseException(f"Signal `{self.signal}` lacks a target")
        if not isinstance(self.struct, set):
            self.struct = set([self.struct])
        if any([not isinstance(x, pp.Forward) for x in self.struct]):
            raise AcabParseException(f"Signal `{self.signal}` isn't paired to a `Forward` Parser")

    def extend(self, spec:PyParse_Spec):
        if not isinstance(spec, PyParse_Spec):
            raise AcabParseException(f"Tried to extend a PyParse_Spec with {spec}")

        self.struct.update(spec.struct)

    def register(self, handler: DSL_Handler_i):
        assert(isinstance(handler, DSL_Handler_i))
        if handler.func is None:
            raise AcabParseException(f"`{self.signal}` attempted to register a handler without a parser")

        # TODO flag logic, api's
        self.handlers.append(handler)

    def __call__(self, *args, **kwargs):
        """
        DSL spec call logic is different from handler system
        """
        return self.build()

    def parseString(self, *args, **kwargs):
        combined = self.build()
        return combined.parseString(*args, **kwargs)

    def build(self):
        #assert(self.flag_e.COLLECT in self.flags)
        results = [x.func for x in self.handlers if x.func is not None]
        output  = pp.Empty()

        # TODO improve logic with flags
        #
        if not bool(results):
            logging.debug(f"No Parsers provided for signal: `{self.signal}`")
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
            # Propagate parser names, even through Forwards
            deep_update_names(struct)
            # Then for next use of parsers to generate their own name again
            clear_parser_names(struct)

        return output

#----------------------------------------
class PyParseDSL(DSL_Builder_i):


    def _register_default(self):
        if "_:_default" in self and bool(self.handler_specs["_:_default"]):
            return

        self._register_spec(PyParse_Spec("_:_default"))
    @EnsureDSLInitialised
    def parse(self, s:str) -> List[Sentence]:
        if not bool(self['_:_default']):
            raise AcabParseException("No Default Parser Set for DSL")

        try:
            return self['_:_default'].parseString(s, parseAll=True)[:]
        except pp.ParseException as exp:
            logging.warning("\n--------------------\nParse Failure:\n")
            traceback.print_tb(exp.__traceback__)
            logging.warning(f"\n{exp.args[-1]}\n")
            logging.warning(f"Parser: {exp.parserElement}\n")
            logging.warning("Line: {}, Col: {} : {}".format(exp.lineno, exp.col, exp.markInputline()))
            return []
