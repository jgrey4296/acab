from datetime import datetime
from enum import Enum
from os.path import split, splitext, exists, expanduser, abspath
import importlib
import logging as root_logger
import pyparsing as pp
import re

import acab
config = acab.setup()

from acab.modules.repl.repl_cmd import register
from acab.modules.repl import ReplParser as RP
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionStructure

logging = root_logger.getLogger(__name__)

# TODO add help flag for print/listen/stats

# TODO shift this into config
SPLIT_RE = re.compile("[ .!?/]")

@register
def do_print(self, line):
    """
    Print out information
    """
    params = RP.printer_parser.parseString(line)[:]
    logging.info(f"Printing: {line}")
    result = []
    if "wm" in params[0]:
        print(self.state.engine.pprint())
    elif "module" in params[0]:
        # TODO print a specific module, or all
        print("Modules: ")
        print("\n".join(self.state.engine._module_loader.loaded_modules.keys()))
    else:
        # TODO print keywords if passed nothing
        # TODO if theres a query, print the value at the end of that query
        print(f"Print Keywords: {RP.printer_parser}")


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
def do_stats(self, line):
    """
    Print Stats about the self.
    ie: Modules/Operators/Pipelines/Layers/Rules....
    """
    logging.info(f"Getting Stats: {line}")
    params = RP.stats_parser.parseString(line)[:]
    allow_all = not bool(params)
    result = []
    # Operators
    if allow_all or "operator" in params:
        result.append("Operator Stats: ")
        # TODO use engine._module_loader.loaded_modules[:].operators
        result.append(str(self.state.engine._operators))

    # pipeline
    if allow_all or "pipeline" in params:
        result.append("--------------------")
        result.append("Pipeline Stats: ")
        # TODO pipeline stats

    # layers
    if (allow_all or "layer" in params):
        result.append("--------------------")
        result.append("Layer Stats: ")
        # result.append("\t{}".format("\n\t".join([str(x) for x in self.state.engine._pipeline._layers])))
        # TODO layer stats

    # agendas
    if allow_all or "agenda" in params:
        result.append("--------------------")
        result.append("Agenda Stats: ")
        # TODO: need to query for agendas

    # rules
    if allow_all or "rule" in params:
        result.append("--------------------")
        result.append("ProductionStructure Stats: ")
        # TODO rule stats

    # modules
    if allow_all or "module" in params:
        result.append("--------------------")
        result.append("Module Stats: ")
        result.append("\t{}".format("\n\t".join([x for x in self.state.engine._loaded_modules])))

    # WM stats
    if allow_all or "wm" in params:
        result.append("--------------------")
        result.append("WM Stats: ")
        # TODO
        result.append(str(self.state.engine._working_memory))

    # Bindings
    if allow_all or "binding" in params:
        result.append("--------------------")
        result.append("Binding Stats: ")
        # TODO pretty print bindings
        bind_groups = self.state.engine._cached_bindings[:]
        result.append("\t{}".format("\n\t".join([str(x) for x in bind_groups])))

    # TODO add parser stats

    # Print Keywords
    if allow_all or "keywords" in params:
        result.append("--------------------")
        result.append("Keywords: ")
        result.append("\t{}".format(" ".join(x for x in ["operator", "agenda", "rule", "pipeline",
                                                         "layer", "module", "wm", "binding", "keywords"])))


    logging.info("\n".join(result))




@register
def do_result(self, line):
    """
    Inspect a result from a query.
    result  -> False or len(ctxs)
    result.SLICE -> Print binding groups for each context in the slice
    result.SLICE.$x -> Print variable $x for each context in slice

    (SLICE == python slice. ie: [:-1], [2:4], [4])
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
        ctxs_to_print.append(self.state.result[params['context'][0]])
    elif "context_slice" in params:
        ctxs_to_print += self.state.result[params['context_slice']]
    else:
        ctxs_to_print.append(self.state.result[0])


    if "bindings" in params:
        bindings_to_print += params.bindings[:]

    logging.info("Printing Result:")
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
def do_decompose(self, line):
    """
    Decompose binding into components.
    eg: rule -> queries, transforms, actions
    """
    params = line
    # TODO : split objects into tries
    # run query
    # split result into bindings

@register
def do_parser(self, line):
    """ Print a parser report """
    # TODO improve
    if "bootstrap" in line:
        print("Bootstrap Parser:")
        print(self.state.engine._dsl_builder._bootstrap_parser.report())
    elif "sugar" in line:
        print(f"Repl Sugar: {RP.sugared}")
    else:
        print(self.state.engine._dsl_builder._main_parser)
