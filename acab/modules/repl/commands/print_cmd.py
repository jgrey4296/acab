#!/usr/bin/env python3
from __future__ import annotations

import abc
import logging as logmod
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)

import pyparsing as pp

logging = logmod.getLogger(__name__)

if TYPE_CHECKING:
    # tc only imports
    pass


from acab.modules.repl.repl_commander import register_class
from acab.modules.repl.ReplParser import rst, ctx_parser

@register_class("print")
class PrintCmd:
    """
    Print out information on the wm, or a module
    [wm, module, semantics]
    """

    def __init__(self):
        self._parser = self._gen_parser()

    def _gen_parser(self):
        wm_kw        = pp.Keyword("wm")("wm")
        mod_target   = pp.Optional(pp.Word(pp.alphas + ".")("mod_target"))
        module_kw    = pp.MatchFirst([pp.Keyword("mod"),
                                      pp.Keyword("module")])("module")
        semantic_kw  = pp.Keyword("semantics")("semantics")


        # TODO also handle bind without kw?
        printer_parser = pp.MatchFirst([wm_kw,
                                        module_kw + mod_target,
                                        semantic_kw,
                                        ctx_parser,
                                        rst])

        return printer_parser

    
    def __call__(self, line):
        try:
            params = self._parser.parse_string(line)
        except pp.ParseException as err:
            logging.warning("Bad Print Command")
            logging.warning(err.markInputline())
            logging.warning("Use: {self._parser}")
            return

        logging.info(f"Printing: {line}")
        result = []
        if "wm" in params:
            # TODO print everything from a query down
            print(self._cmd.state.engine.pprint())
        elif "mod_target" in params:
            print("TODO Specific Module Info: ")
            # TODO print module doc
            modules: ModuleComponents = self._cmd.state.engine._module_loader.loaded_modules.values()
            modules = [x for x in modules if params['mod_target'] in x.source]
            print("\n".join([x.source for x in modules]))
        elif "module" in params:
            print("Modules: ")
            modules: ModuleComponents = self._cmd.state.engine._module_loader.loaded_modules.values()
            print("\n".join([x.source for x in modules]))
        elif "context" in params or "short_context" in params or "context_slice" in params:
            self.print_contexts(params)
        elif "semantics" in params:
            print("TODO Semantic Printing not implemented yet")
        else:
            print(f"Print Keywords: {self._parser}")

    def print_contexts(self, params):
        ctxs_to_print     = []
        bindings_to_print = []
        if "short_context" in params:
            try:
                ctxs_to_print.append(self._cmd.state.ctxs[params['short_context']])
            except IndexError as err:
                print(f"Selected bad ctx instance. Try 0 <= x < {len(self._cmd.state.ctxs)}.")

        elif "context_slice" in params:
            ctx_slice = self._cmd.state.ctxs[params['context_slice']].active_list()
            ctxs_to_print += ctx_slice
        elif bool(self._cmd.state.ctxs) and len(self._cmd.state.ctxs) > 0:
            ctxs_to_print += self._cmd.state.ctxs.active_list()
        else:
            print(f"No applicable contexts to print")
            return

        if "bindings" in params:
            bindings_to_print += params.bindings[:]

        logging.info("Ctxs: {}", ctxs_to_print)
        logging.info("Bindings: {}", bindings_to_print)

        # now print them
        for i,ctx in enumerate(ctxs_to_print):
            if len(ctxs_to_print) > 1:
                print(f"Context: {i}")
            else:
                print("Context: ")
            # if bool(ctx.continuation):
            #     print(f"Continuation: {ctx.continuation}")
            if bool(bindings_to_print):
                for x in bindings_to_print:
                    print("{} : {}".format(x, self._cmd.state.engine.pprint(target=[ctx[x]])))
            else:
                for x,y in ctx.data.items():
                    print("{} : {}".format(x, self._cmd.state.engine.pprint(target=[y])))

            print("--------------------")

        if bool(self._cmd.state.ctxs._named_sets):
            print("Named (continuation) Sets:")
            print(self._cmd.state.engine.pprint(target=list(self._cmd.state.ctxs._named_sets.keys())))
