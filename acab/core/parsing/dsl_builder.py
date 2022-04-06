#!/usr/bin/env python3
import abc
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Generic, Iterable,
                    Iterator, Mapping, Match, MutableMapping, Protocol,
                    Sequence, Tuple, TypeAlias, TypeGuard, TypeVar, cast)

import logging as logmod
logging = logmod.getLogger(__name__)

from acab.interfaces import dsl
from acab.interfaces import handler_system as HS

if TYPE_CHECKING:
    # tc only imports
    pass

from acab import types as AT
from acab.core.util.part_implementations import handler_system as HSImpl
from acab.error.parse import AcabParseException

Parser           : TypeAlias = AT.Parser
Sentence         : TypeAlias = AT.Sentence
Query            : TypeAlias = AT.Container
ModuleComponents : TypeAlias = AT.ModuleComponents
DSL_Spec_A       : TypeAlias = AT.DSL_Spec

class DSL_Fragment(HSImpl.HandlerFragment, dsl.DSL_Fragment_i):
    pass

class DSL_Spec(HSImpl.HandlerSpec, dsl.DSL_Spec_i):

    def extend_spec(self, spec:DSL_Spec_A):
        if not isinstance(spec, dsl.DSL_Spec_i):
            raise AcabParseException(f"Tried to extend a DSL_Spec with {spec}")

        self.struct.update(spec.struct) #type:ignore

    def register(self, handler):
        assert(isinstance(handler, dsl.DSL_Handler_i))
        if handler.func is None:
            raise AcabParseException(f"`{self.signal}` attempted to register a handler without a parser")

        # TODO flag logic, api's
        self.handlers.append(handler)


class DSL_Builder(HSImpl.HandlerSystem, dsl.DSL_Builder_i):

    def _register_spec(self, *specs):
        for spec in specs:
            assert(isinstance(spec, dsl.DSL_Spec_i))
            if spec.signal not in self.handler_specs:
                self.handler_specs[spec.signal] = spec
            else:
                self.handler_specs[spec.signal].extend_spec(spec)

    def build(self):
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

    def parse_string(self, *args) -> list[Sentence]:
        return self.parse(*args)

    def parse_file(self, f:str) -> list[Sentence]:
        logging.debug(f"Loading File: {f}")
        text = ""
        with open(f, "r") as the_file:
            text = the_file.read()

        logging.info(f"Loading File Text:\n{text}")
        return self.parse(text)


    def __call__(self, *args):
        return self.parse(*args)

    def extend(self, modules:list[ModuleComponents]):
        for module in modules:
            self.register(*module.dsl_fragments)

    def __repr__(self):
        return f"<{self.__class__.__name__}: Handlers: {len(self.handler_specs)}, Loose: {len(self.loose_handlers)}>"
