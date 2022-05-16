#!/usr/bin/env python3
from __future__ import annotations

import abc
import logging as logmod
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from collections import defaultdict
import pyparsing as pp

logging = logmod.getLogger(__name__)

if TYPE_CHECKING:
    # tc only imports
    pass


from acab.modules.repl.repl_commander import register_class


@register_class("stat")
class StatCmd:
    """
    Print Stats about the self._cmd.
    ie: Modules/Operators/Pipelines/Layers/Rules....
    """

    def __init__(self):
        self._parser = self._gen_parser()

    def _gen_parser(self):
        operator_kw  = pp.MatchFirst([pp.Keyword("ops"),
                                      pp.Keyword("operator")])("operator")
        module_kw    = pp.MatchFirst([pp.Keyword("mod"),
                                      pp.Keyword("module")])("module")
        semantic_kw  = pp.Keyword("semantics")("semantics")
        printer_kw   = pp.Keyword("printers")("printers")

        stats_parser = pp.ZeroOrMore(pp.MatchFirst([operator_kw,
                                                    module_kw,
                                                    semantic_kw,
                                                    printer_kw]))
        return stats_parser

    def __call__(self, line):
        self.do_stat(line)

    def do_stat(self, line):
        logging.info(f"Getting Stats: {line}")
        params                              = self._parser.parse_string(line)
        allow_all    : bool                 = not bool(params)
        self.modules : list[ModuleFragment] = self._cmd.state.engine._module_loader.loaded
        # modules
        if allow_all or "module" in params:
            self.pr_modules(params)

        if allow_all or "semantics" in params:
            self.pr_semantics(params)

        if allow_all or "operator" in params:
            self.pr_operators(params)

        if allow_all or "printers" in params:
            self.pr_printers(params)

    def pr_modules(self, params):
        print("\n--------------------")
        print("MODULES: ")
        print("--------------------")
        mod_target = self.modules
        if 'mod_target' in params:
            mod_target = [x for x in self.modules if x.source in params['mod_target']]

        for mod in mod_target:
            print("\t{}".format(mod))
        print("--")
        print("Loaded Modules: {}".format(len(self.modules)))


    def pr_semantics(self, params):
        print("\n--------------------")
        print("SEMANTICS:")
        print("--------------------")
        semantic = self._cmd.state.engine.semantics
        print("{} : {}".format(semantic.__module__, semantic.__class__.__name__))
        print("\n", semantic.__doc__, "\n")
        print(f"{repr(semantic)}\n")
        print("Handlers: {}".format(len(semantic.handler_specs)))
        print("\t{}".format("\n\t".join(sorted([str(x) for x in semantic.handler_specs.values()]))))

        print("----------")
        mods_with_semantics = [x for x in self.modules if len(x.semantics) > 0]
        if bool(mods_with_semantics):
            print("Module Semantics: ")
            count = defaultdict(lambda: 0)
            for mod in mods_with_semantics:
                print(f"Module: {mod.source}")
                for frag in mod.semantics:
                    print(f"\t{frag}")

    def pr_operators(self, params):
        print("\n--------------------")
        print("OPERATORS: ")
        print("--------------------")
        count = 0
        for mod in self.modules:
            count += len(mod.operators)
            for op in mod.operators:
                print("\t", self._cmd.state.engine.pprint(target=[op]))

        print("--")
        print("Loaded Operators: {}".format(count))
        # TODO print Context Operator Bindings


    def pr_printers(self, params):
        print("\n--------------------")
        print("PRINTERS:")
        print("--------------------")
        printer = self._cmd.state.engine.printer
        print("{} : {}".format(printer.__module__, printer.__class__.__name__))
        print("\n", printer.__doc__, "\n")
        print(f"{repr(printer)}\n")
        print("Handlers: {}".format(len(printer.handler_specs)))
        print("\t{}".format("\n\t".join(sorted([str(x) for x in printer.handler_specs.values()]))))

        print("----------")
        mods_with_printers = [x for x in self.modules if len(x.printers) > 0]
        if bool(mods_with_printers):
            print("Module Printers: ")
            count = defaultdict(lambda: 0)
            for mod in mods_with_printers:
                print(f"Module: {mod.source}")
                for frag in mod.printers:
                    print(f"{frag}")
