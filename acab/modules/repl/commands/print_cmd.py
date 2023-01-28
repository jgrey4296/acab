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
from acab import types as AT

ModuleFragment = AT.ModuleFragment

@register_class("print")
class PrintCmd:
    """
    Print out details of the current state.

    Usage:
    print wm                : prints the current working memory
    print (ctx|c)           : prints the current active context instances
    print ctx[{num}]        : prints a specific context instance by index
    print ctx[{num}:{num}]  : prints a slice of the context instances
    print ctx {binding*}    : prints a specific binding from the context instances

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
            params = self._parser.parse_string(line, parse_all=True)
        except pp.ParseException as err:
            print("Bad Print Command: ", err.pstring)
            print(self.__doc__)
            return

        logging.info(f"Printing: {line}")
        result = []
        if "wm" in params:
            # TODO print everything from a query down
            print(self._repl.state.engine.pprint())
        elif "context" in params or "short_context" in params or "context_slice" in params:
            self.prep_print_contexts(params)

    def prep_print_contexts(self, params):
        ctxs_to_print     = []
        bindings_to_print = []

        # Get the contexts to print
        if "short_context" in params:
            try:
                ctxs_to_print.append(self._repl.state.ctxs[params['short_context']])
            except IndexError as err:
                print(f"Selected bad ctx instance. Try 0 <= x < {len(self._repl.state.ctxs)}.")

        elif "context_slice" in params:
            ctx_slice = self._repl.state.ctxs[params['context_slice']].active_list()
            ctxs_to_print += ctx_slice
        elif bool(self._repl.state.ctxs) and len(self._repl.state.ctxs) > 0:
            ctxs_to_print += self._repl.state.ctxs.active_list()
        elif bool(self._repl.state.ctxs._named_sets):
            print("Named (continuation) Sets:")
            print(self._repl.state.engine.pprint(target=list(self._repl.state.ctxs._named_sets.keys())))

        if not bool(ctxs_to_print):
            print(f"No applicable contexts to print")
            return

        if "bindings" in params:
            bindings_to_print += params.bindings[:]

        self.print_contexts(params, ctxs_to_print, bindings_to_print)

    def print_contexts(self, params, ctxs_to_print, bindings):
        logging.info("Ctxs: {}", ctxs_to_print)
        logging.info("Bindings: {}", bindings)
        # now print them
        for i,ctx in enumerate(ctxs_to_print):
            if len(ctxs_to_print) > 1:
                print(f"Context: {i}")
            else:
                print("Context: ")
            # if bool(ctx.continuation):
            #     print(f"Continuation: {ctx.continuation}")
            if bool(bindings):
                for x in bindings:
                    print("{:<5} : {}".format(x, self._repl.state.engine.pprint(target=[ctx[x]])))
            else:
                for x,y in ctx.data.items():
                    print("{:<5} : {}".format(x, self._repl.state.engine.pprint(target=[y])))

            print("--------------------")

