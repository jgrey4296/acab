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

from acab.modules.repl.repl_cmd import register
from acab.modules.repl import ReplParser as RP
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionStructure

logging = root_logger.getLogger(__name__)

# TODO shift this into config
SPLIT_RE = re.compile("[ .!?/]")
ModuleComponents = "ModuleComponents"

@register
def do_print(self, line):
    """
    Print out information
    """
    try:
        params = RP.printer_parser.parseString(line)[:]
    except pp.ParseException as err:
        logging.warning("Bad Print Command")
        logging.warning(err.markInputline())
        logging.warning("Use: {RP.printer_parser}")
        return

    logging.info(f"Printing: {line}")
    result = []
    if "wm" in params:
        print(self.state.engine.pprint())
    elif "module" in params:
        print("Modules: ")
        modules: ModuleComponents = self.state.engine._module_loader.loaded_modules.values()
        if 'mod_target' in params:
            # TODO print module doc
            modules = [x for x in modules if x.source in params['mod_target']]

        print("\n".join([x.source for x in modules]))
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

    if allow_all or "semantic" in params:
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
def do_print_ctx(self, line):
    """
    Inspect active contexts from a query.
    result (index|SLICE)? var*

    """
    try:
        params = RP.result_parser.parseString(line)
    except pp.ParseException as err:
        logging.warning("Bad Results Command:")
        logging.warning(err.markInputline())
        return

    ctxs_to_print     = []
    bindings_to_print = []
    logging.info("Params: {}".format(params))
    if "context" in params:
        try:
            ctxs_to_print.append(self.state.result[params['context']])
        except IndexError as err:
            print(f"Selected bad ctx instance. Try 0 <= x < {len(self.state.result)}.")
    elif "context_slice" in params:
        ctxs_to_print += self.state.result[params['context_slice']]
    elif bool(self.state.result) and len(self.state.result) > 0:
        ctxs_to_print.append(self.state.result[0])
    else:
        print("No Results Exist. Perform a Query.")
        return

    if "bindings" in params:
        bindings_to_print += params.bindings[:]

    logging.info("Ctxs: {}".format(ctxs_to_print))
    logging.info("Bindings: {}".format(bindings_to_print))

    # now print them
    for ctx in ctxs_to_print:
        if bool(bindings_to_print):
            for x in bindings_to_print:
                print("{} : {}".format(x, self.state.engine.pprint([ctx[x]])))
        else:
            for x,y in ctx.data.items():
                print("{} : {}".format(x, self.state.engine.pprint([y])))

        print("--------------------")


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
