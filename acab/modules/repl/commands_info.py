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

logging = root_logger.getLogger(__name__)

# TODO shift this into config
SPLIT_RE = re.compile("[ .!?/]")
ModuleComponents = "ModuleComponents"

@register
def do_print(self, line):
    """
    Print out information on the wm, or a module
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
        print("Modules: ")
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
    # Operators
    if allow_all or "operator" in params:
        print("--------------------")
        print("Operators: ")
        count = 0
        for mod in modules:
            count += len(mod.operators)
            for op in mod.operators:
                print("\t", self.state.engine.pprint([op]))

        print("--")
        print("Loaded Operators: {}".format(count))

    # modules
    if allow_all or "module" in params:
        print("--------------------")
        print("Modules: ")
        mod_target = modules
        if 'mod_target' in params:
            mod_target = [x for x in modules if x.source in params['mod_target']]

        for mod in mod_target:
            print("\t{}".format(mod.source))
        print("--")
        print("Loaded Modules: {}".format(len(modules)))

    if allow_all or "semantics" in params:
        print("--------------------")
        print("Base Semantics:")
        semantic = self.state.engine.semantics
        print("{} : {}".format(semantic.__module__, semantic.__class__.__name__))
        print("\n", semantic.__doc__, "\n")
        print("Handlers: {}".format(len(semantic.registered_handlers)))
        print("Structs:  {}".format(len(semantic.registered_structs)))

        print("Handler Keys:")
        print("\t{}".format("\n\t".join([str(x) for x in semantic.registered_handlers.keys()])))

        print("----------")
        print("Module Semantics: ")
        count = defaultdict(lambda: 0)
        for mod in modules:
            print("Module: {} : Fragments: {}".format(mod.source, len(mod.semantics)))
            for frag in mod.semantics:
                count['dependent']   += len(frag.dependent)
                count['independent'] += len(frag.independent)
                count['abstraction'] += len(frag.abstraction)
                count['structs']     += len(frag.structs)

        if bool(count):
            print("--")
            print("Semantic Counts:")
            print("\n\t".join(["{} : {}".format(x,y) for x,y in count.items()]))



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
    if bool(line):
        parser = self.state.engine._dsl_builder._bootstrap_parser.query(line)
        if bool(parser):
            parser.setDebug(not parser.debug)
            print(f"Debug {parser.debug} : {parser}")
    else:
        curr = self.state.engine._dsl_builder._main_parser.debug
        self.state.engine._dsl_builder._main_parser.setDebug(not curr)
        print(f"Parser Debug: {not curr}")

@register
def do_listen(self, line):
    """ Listeners:
    Listen for specific assertions / rule firings / queries,
    and pause on them
    """
    logging.info(f"Listening: {line}")
    params = RP.listen_parser.parseString(line)[:]
    words = [y for x in params for y in SPLIT_RE.split(x)]
    if params['type'] == "add":
        self.state.engine.add_listeners(*words)
    elif params['type'] == "remove":
        self.state.engine.remove_listeners(*words)
    elif params['type'] == "list":
        result = []
        result.append(" ".join(self.state.engine.get_listeners()))
        result.append("Threshold: {}".format(self.state.engine.get_listener_threshold()))
        params["result"] = "\n".join(result)
    elif params['type'] == "threshold":
        params = SPLIT_RE.split((params[0]))
        self.state.engine.set_listener_threshold(int(params[0]), int(params[1]))



@register
def do_check(self, line):
    """
    Trigger an analysis action.
    eg: type checking
    """
    params = line
    logging.info(f"Checking: {line}")
    # TODO
    # If single statement, run analysis layer with statement inserted,
    # return types

    # else everything: run analysis layer




@register
def do_decompose(self, line):
    """
    Decompose binding into components.
    eg: rule -> queries, transforms, actions
    """
    params = line
    # TODO : split objects into tries
    # run query
    # split result into bindings
