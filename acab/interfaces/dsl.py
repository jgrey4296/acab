"""
A DSL interface for the system, which

"""
import abc
import collections.abc as cABC
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
from acab.interfaces.handler_system import (Handler, Handler_Fragment,
                                            HandlerSpec, HandlerSystem_i)
from acab.interfaces.handler_system import Handler_Fragment, Handler, HandlerSpec
from acab.core.parsing.funcs import deep_update_names, clear_parser_names

Parser           = "pp.ParserElement"
Sentence         = AT.Sentence
Query            = AT.Container
ModuleComponents = AT.ModuleComponents
DSL_Spec         = "DSL_Spec"
File             = 'FileObj'

#----------------------------------------
class DSL_Fragment(Handler_Fragment):
    """ """

    def parseString(self, string):
        """ Takes a String, parses it into Data format """
        # TODO maybe create a parser on demand of all handlers in the fragment?
        raise NotImplementedError()

    def parse_file(self, file):
        raise NotImplementedError()


class DSL_Handler(Handler):
    """ Register a function for handling a DSL setup signal.
    ie: This function is run to set a pyparsing `Forward`"""

    func : Parser = field()
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
class DSL_Spec(HandlerSpec):
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

    def extend(self, spec:DSL_Spec):
        if not isinstance(spec, DSL_Spec):
            raise AcabParseException(f"Tried to extend a DSL_Spec with {spec}")

        self.struct.update(spec.struct)

    def register(self, handler: DSL_Handler):
        assert(isinstance(handler, DSL_Handler))
        if handler.func is None:
            raise AcabParseException(f"`{self.signal}` attempted to register a handler without a parser")

        # TODO flag logic, api's
        self.handlers.append(handler)

    def __call__(self, *args, **kwargs):
        """
        DSL spec call logic is different from handler system
        """
        # TODO improve logic with flags
        #
        #assert(self.flag_e.COLLECT in self.flags)
        results = [x.func for x in self.handlers if x.func is not None]
        output  = pp.Empty()

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

    def parseString(self, *args, **kwargs):
        combined = pp.MatchFirst([x.func for x in self.handlers if x.func is not None])
        return combined.parseString(*args, **kwargs)

    def build(self):
        return pp.MatchFirst([x.func for x in self.handlers if x.func is not None])

#----------------------------------------
@dataclass
class DSLBuilder(HandlerSystem_i):

    _parsers_initialised  : bool           = field(init=False, default=False)
    _loaded_DSL_fragments : Dict[Any, Any] = field(init=False, default_factory=dict)

    def _register_spec(self, *specs: DSL_Spec):
        for spec in specs:
            if spec.signal not in self.handler_specs:
                self.handler_specs[spec.signal] = spec
            else:
                self.handler_specs[spec.signal].extend(spec)

    def _register_default(self):
        if "_:_default" in self and bool(self.handler_specs["_:_default"]):
            return

        self._register_spec(DSL_Spec("_:_default"))



    def build_DSL(self):
        """
        Using currently loaded modules, rebuild the usable DSL parser from fragments
        """
        if bool(self.loose_handlers):
            loose_handlers      = self.loose_handlers[:]
            self.loose_handlers = []
            for loose in loose_handlers:
                self.register(loose)

        if bool(self.loose_handlers):
            loose_signals = ", ".join(set([x.signal for x in self.loose_handlers]))
            logging.warning(f"Building DSL with {len(self.loose_handlers)} Loose Handlers: {loose_signals}")
        else:
            logging.info("Building DSL")

        for spec in self:
            try:
                spec()
            except TypeError as err:
                breakpoint()

        self._parsers_initialised = True

    def parseString(self, *args):
        return self.parse(*args)
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

    @EnsureDSLInitialised
    def parseFile(self, f:File) -> List[Sentence]:
        logging.debug(f"Loading File: {f}")
        text = ""
        with open(f, "r") as the_file:
            text = the_file.read()

        logging.info(f"Loading File Text:\n{text}")
        return self.parse(text)


    def __call__(self, *args):
        return self.parse(*args)

    def extend(self, modules:List[ModuleComponents]):
        for module in modules:
            self.register(*module.dsl_fragments)
