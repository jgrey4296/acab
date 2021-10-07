from datetime import datetime
from enum import Enum
from os.path import split, splitext, exists, expanduser, abspath
import importlib
import logging as root_logger
import pyparsing as pp
import re
from collections import defaultdict
import acab
config = acab.setup()

from acab.modules.repl.repl_commander import register
from acab.modules.repl import ReplParser as RP
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionStructure
from acab.modules.repl.util import print_contexts
from acab.abstract.parsing import debug_funcs as DBF

logging = root_logger.getLogger(__name__)

# TODO shift this into config
SPLIT_RE = re.compile("[ .!?/]")
ModuleComponents = "ModuleComponents"

@register
def do_print(self, line):
    """
    Print out information on the wm, or a module
    [wm, module, semantics]
    """
    try:
        params = RP.printer_parser.parseString(line)
    except pp.ParseException as err:
        logging.warning("Bad Print Command")
        logging.warning(err.markInputline())
        logging.warning("Use: {RP.printer_parser}")
        return

    logging.info(f"Printing: {line}")
    result = []
    if "wm" in params:
        # TODO print everything from a query down
        print(self.state.engine.pprint())
    elif "mod_target" in params:
        print("TODO Specific Module Info: ")
        # TODO print module doc
        modules: ModuleComponents = self.state.engine._module_loader.loaded_modules.values()
        modules = [x for x in modules if params['mod_target'] in x.source]
        print("\n".join([x.source for x in modules]))
    elif "module" in params:
        print("Modules: ")
        modules: ModuleComponents = self.state.engine._module_loader.loaded_modules.values()
        print("\n".join([x.source for x in modules]))
    elif "context" in params or "short_context" in params or "context_slice" in params:
        print_contexts(self, params)
    elif "semantics" in params:
        print("Semantic Printing not implemented yet")
        raise NotImplementedError()
    else:
        print(f"Print Keywords: {RP.printer_parser}")

@register
def do_stat(self, line):
    """
    Print Stats about the self.
    ie: Modules/Operators/Pipelines/Layers/Rules....
    """
    logging.info(f"Getting Stats: {line}")
    params = RP.stats_parser.parseString(line)
    allow_all = not bool(params)
    modules: ModuleComponents = self.state.engine._module_loader.loaded_modules.values()
    # modules
    if allow_all or "module" in params:
        print("\n--------------------")
        print("MODULES: ")
        print("--------------------")
        mod_target = modules
        if 'mod_target' in params:
            mod_target = [x for x in modules if x.source in params['mod_target']]

        for mod in mod_target:
            print("\t{}".format(mod))
        print("--")
        print("Loaded Modules: {}".format(len(modules)))

    if allow_all or "semantics" in params:
        print("\n--------------------")
        print("SEMANTICS:")
        print("--------------------")
        semantic = self.state.engine.semantics
        print("{} : {}".format(semantic.__module__, semantic.__class__.__name__))
        print("\n", semantic.__doc__, "\n")
        print(f"{repr(semantic)}\n")
        print("Handlers: {}".format(len(semantic.handlers)))
        print("\t{}".format("\n\t".join([str(x) for x in semantic.handlers.values()])))

        print("----------")
        mods_with_semantics = [x for x in modules if len(x.semantics) > 0]
        if bool(mods_with_semantics):
            print("Module Semantics: ")
            count = defaultdict(lambda: 0)
            for mod in mods_with_semantics:
                print(f"Module: {mod.source}")
                for frag in mod.semantics:
                    print(f"{frag}")

    # Operators
    if allow_all or "operator" in params:
        print("\n--------------------")
        print("OPERATORS: ")
        print("--------------------")
        count = 0
        for mod in modules:
            count += len(mod.operators)
            for op in mod.operators:
                print("\t", self.state.engine.pprint([op]))

        print("--")
        print("Loaded Operators: {}".format(count))
        # TODO print Context Operator Bindings


    if allow_all or "printers" in params:
        print("\n--------------------")
        print("PRINTERS:")
        print("--------------------")
        printer = self.state.engine.printer
        print("{} : {}".format(printer.__module__, printer.__class__.__name__))
        print("\n", printer.__doc__, "\n")
        print(f"{repr(printer)}\n")
        print("Handlers: {}".format(len(printer.handlers)))
        print("\t{}".format("\n\t".join([str(x) for x in printer.handlers.values()])))

        print("----------")
        mods_with_printers = [x for x in modules if len(x.printers) > 0]
        if bool(mods_with_printers):
            print("Module Printers: ")
            count = defaultdict(lambda: 0)
            for mod in mods_with_printers:
                print(f"Module: {mod.source}")
                for frag in mod.printers:
                    print(f"{frag}")

    # TODO Working memory / structures / memory load

@register
def do_parser(self, line):
    """ Print a parser report.
    Defaults to primary, can take:
    bootstrap,
    sugar
    """
    params = RP.parse_info_parser.parseString(line)

    if "bootstrap" in line:
        print("Bootstrap Parser:")
        bootstrap_desc = self.state.engine._dsl_builder._bootstrap_parser.report()
        for sen in bootstrap_desc:
            print("\t", self.state.engine.pprint([sen]))
    elif "sugar" in line:
        print(f"Repl Sugar: {RP.sugared}")
    else:
        print("Top Level ACAB Parser:")
        print(self.state.engine._dsl_builder._main_parser)

@register
def do_debug(self, line):
    """ Toggle Debugging of pyparsing """
    if bool(line):
        parser = self.state.engine._dsl_builder._bootstrap_parser.query(line)
        if bool(parser):
            parser.setDebug(not parser.debug)
            print(f"Debug {parser.debug} : {parser}")
    else:
        DBF.debug_pyparsing()


# Logging Control###############################################################
@register
def do_log(self, line):
    """ Change the logging level """
    try:
        root = root_logger.getLogger('')
        handler = root.handlers[1]
        if bool(line):
            level = root_logger._nameToLevel[line.upper()]
            root.setLevel(level)
            handler.setLevel(level)
            print(f"Set Console Log level to: {line.upper()} : {level}")
        else:
            level = handler.level
            if handler.level in root_logger._levelToName:
                level = root_logger._levelToName[handler.level]
            print(f"Console Log Level: {level}")

    except KeyError as err:
        print(f"Unrecognised Log Level: {line.upper()}")

@register
def do_fmt(self, line):
    """ Change the Log format """
    root = root_logger.getLogger('')
    handler = root.handlers[1]
    if bool(line):
        handler.setFormatter(root_logger.Formatter(line))
        print(f"Set Console Log Format to: {line}")
    else:
        fmt = handler.formatter._fmt
        print(f"Console Log Level: {fmt}")
        print(f"See https://docs.python.org/3/library/logging.html#logrecord-attributes")

@register
def do_filter(self, line):
    """ Add or remove a logging filter """
    root = root_logger.getLogger('')
    handler = root.handlers[1]
    if bool(line):
        handler.addFilter(root_logger.Filter(line))
        print(f"Set Console Log Format to: {line}")
    elif bool(handler.filters):
        handler.removeFilter(handler.filters[-1])
        print(f"Removing Last Console Log Filter")
    else:
        print("No filters set")


# Tutorial ####################################################################
@register
def do_tutorial(self, line):
    """
    Print out a basic tutorial of Acab and this Repl
    """
    # Print a section, return to main loop,
    # if tutorial is called again, continue
    # if restart is passed in, restart the tutorial
    return

@register
def do_acab(self, line):
    print("All Cops Are Bastards")
