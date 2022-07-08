#!/usr/bin/env python3
# pylint: disable=no-member
from __future__ import annotations

import abc
import logging as logmod
from collections import defaultdict
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from os.path import splitext

import pyparsing as pp
from acab import AcabConfig
from acab.modules.repl.repl_commander import register_class
from acab_config.utils.log_formatter import SimpleLogColour
import acab.modules.repl.commands.util as ColPr
from acab.interfaces.fragments import ModuleFragment

logging = logmod.getLogger(__name__)
SC      = SimpleLogColour

config = AcabConfig()
import_dict = config.prepare("Imports.Targeted", _type=dict)

@register_class("report")
class ReportCmd:
    """
    Print reports about the system configurationz

    Prints all information by default,
    If you type `report {section}` prints only that section.

    Available sections:
    dsl
    engine
    module   / mod
    operator / ops
    printers
    semantics

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
        dsl_kw       = pp.Keyword("dsl")("dsl")
        engine_kw    = pp.Keyword("engine")("engine")

        eol          = pp.line_end("all")

        report_parser = pp.ZeroOrMore(pp.MatchFirst([operator_kw,
                                                    module_kw,
                                                    semantic_kw,
                                                    printer_kw,
                                                    dsl_kw,
                                                    engine_kw,
                                                    eol]))
        return report_parser

    def __call__(self, line):
        self.do_report(line)

    def do_report(self, line):
        logging.info(f"Getting reports: {line}")
        try:
            params                              = self._parser.parse_string(line, parse_all=True)
        except pp.ParseException as err:
            print(f"Unrecognised Command detail: {err.pstr}")
            print(self.__doc__)
            return

        allow_all    : bool                 = (not bool(params)) or "all" in params
        self.modules : list[ModuleFragment] = self._cmd.state.engine._module_loader.loaded
        # modules
        if allow_all or "module" in params:
            self.print_modules(params)

        if allow_all or "dsl" in params:
            self.print_dsl(params)

        if allow_all or "semantics" in params:
            self.print_semantics(params)

        if allow_all or "operator" in params:
            self.print_operators(params)

        if allow_all or "printers" in params:
            self.print_printers(params)

        if allow_all or "engine" in params:
            self.print_engine(params)

    def print_modules(self, params):
        ColPr.print_header("Modules")
        mod_target = self.modules
        if 'mod_target' in params:
            mod_target = [x for x in self.modules if x.source in params['mod_target']]

        for mod in mod_target:
            ColPr.print_module_colour(mod)
        print("--")
        print(f"Loaded Modules: {SC.green(len(self.modules))}")


    def print_dsl(self, params):
        ColPr.print_header("DSL")
        dsl = self._cmd.state.engine._dsl
        ColPr.print_class_colour("DSL Base", dsl)
        ColPr.print_handler_system(dsl)

    def print_semantics(self, params):
        ColPr.print_header("Semantics")
        semantic = self._cmd.state.engine.semantics
        ColPr.print_class_colour("Semantic Base", semantic)
        ColPr.print_handler_system(semantic)

        print("----------")
        mods_with_semantics = [x for x in self.modules if len(x.semantics) > 0]
        if bool(mods_with_semantics):
            print("Module Semantics: ")
            count = defaultdict(lambda: 0)
            for mod in mods_with_semantics:
                print(f"Module: {mod.source}")
                for frag in mod.semantics:
                    print(f"\t{frag}")

    def print_operators(self, params):
        ColPr.print_header("Operators")
        count = 0
        for mod in self.modules:
            count += len(mod.operators)
            for op in mod.operators:
                print("\t", ColPr.two_part_split(self._cmd.state.engine.pprint(target=[op])))

        print("--")
        print(f"Loaded Operators: {SC.green(count)}")
        # TODO print Context Operator Bindings


    def print_printers(self, params):
        ColPr.print_header("Printers")
        printer = self._cmd.state.engine.printer
        ColPr.print_class_colour("Printing Base", printer)
        ColPr.print_handler_system(printer)

        print("----------")
        mods_with_printers = [x for x in self.modules if len(x.printers) > 0]
        if bool(mods_with_printers):
            print("Module Printers: ")
            count = defaultdict(lambda: 0)
            for mod in mods_with_printers:
                print(f"Module: {mod.source}")
                for frag in mod.printers:
                    print(f"{frag}")

    def print_engine(self, params):
        ColPr.print_header("Engine")
        engine = self._cmd.state.engine
        ColPr.print_class_colour("Active Engine", engine)
        load_paths = '\n\t'.join(engine.load_paths) or SC.red("None")
        print(f"\nLoad Paths:\n\t{load_paths}")

        print(f"\nSystem Wide Imports:")
        for k,v in sorted(import_dict().items()):
            key_str = SC.blue(f"{k:<20}")
            path_split = splitext(v)
            print(f"\t{key_str} : {path_split[0]}{SC.blue(path_split[1])}")
