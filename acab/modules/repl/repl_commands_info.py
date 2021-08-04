from datetime import datetime
from enum import Enum
from os.path import split, splitext, exists, expanduser, abspath
import importlib
import logging as root_logger
import re

import acab
config = acab.GET()

from repl_cmd import register
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionStructure

logging = root_logger.getLogger(__name__)

# TODO shift this into config
SPLIT_RE = re.compile("[ .!?/]")

@register
def do_print(self, line):
    """
    Print out information
    """
    params = line
    logging.info("Printing: {}".format(params))
    result = []
    if "wm" in params[0]:
        result.append("WM:")
        result.append(str(self.state.engine._working_memory))
    elif "bootstrap" in params[0]:
        result.append("Bootstrap Parser:")
        result.append(self.state.engine._working_memory._bootstrap_parser.print_trie())

    elif "module" in params[0]:
        result.append("Module: {}".format(params[1]))
        result.append(str(self.state.engine._loaded_modules[params[1]]))
    elif "layer" in params[0]:
        result.append("Layer: {}".format(params[1]))
        result.append(str(self.state.engine._pipeline[params[1]]))
    elif "pipeline" in params[0]:
        result.append("Pipeline: {}".format(params[1]))
        result.append(str(self.state.engine._pipeline))
    elif "binding" in params[0]:
        if len(params) > 1:
            if isinstance(params[1], int) and len(self.state.engine._cached_bindings) <= params[1]:
                result.append("Bindings: {} Out of Bounds".format(params[1]))
            else:
                result.append("Bindings: {}".format(params[1]))
                result.append(str(self._cached_bindings[params[1]]))
        else:
            result.append("Bindings: ")
            result.append("\n".join([str(x) for x in self.state.engine._cached_bindings]))
    else:
        result.append("Querying: {}")

    self.state.result = "\n".join(result)
    # TODO print keywords if passed nothing



@register
def do_decompose(self, line):
    """
    Decompose an object into a trie of its components.
    Useful for decomposing rules
    """
    params = line
    # TODO : split objects into tries
    # run query
    # split result into bindings


@register
def do_listen(self, line):
    """ Listeners:
    Listen for specific assertions / rule firings / queries,
    and pause on them
    """
    params = line
    logging.info("Listener ({}): {}".format(params["type"], params))
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
def do_type_check(self, line):
    """
    Trigger the type checking of the working memory state
    """
    params = line
    logging.info("Type Checking: {}".format(params))
    # TODO
    # If single statement, run analysis layer with statement inserted, return types



    # else everything: run analysis layer



@register
def do_stats(self, line):
    """
    Print Stats about the self.
    ie: Modules/Operators/Pipelines/Layers/Rules....
    """
    params = line
    logging.info("Getting Stats: {}".format(params))
    allow_all = not bool(params)
    result = []
    # Operators
    if allow_all or "operator" in params:
        result.append("Operator Stats: ")
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


    result.append("")
    self.state.result = "\n".join(result)


@register
def do_echo(self, line):
    """
    Toggle echoing of working memory state
    """
    self.state.echo = not self.state.echo


@register
def do_break(self, line):
    """
    Manually switch to PDB for debugging
    """
    debug()
