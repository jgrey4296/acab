"""
Commands for the REPL

"""
from datetime import datetime
from enum import Enum
from os.path import split, splitext, exists, expanduser, abspath
import importlib
import logging as root_logger
import re

import acab
config = acab.GET()

from repl_cmd import AcabREPL
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionStructure

logging = root_logger.getLogger(__name__)

# TODO shift this into config
SPLIT_RE = re.compile("[ .!?/]")

def register(fn):
    """ Decorator for registering a function into the repl """
    assert("do_" in fn.__name__)
    assert(fn.__name__ not in AcabREPL)
    setattr(AcabREPL, fn.__name__, fn)
    # Don't return fn as its only used in the class

#--------------------

@register
def do_init(self, line):
    """
    Specify the Engine to initialise.
    Imports the module, and uses the final component as the Engine Class.
    eg: acab.engines.trie_engine.TrieEngine -> TrieEngine
    """
    # TODO parse this
    params = line
    logging.info("Initialising: {}".format(params))
    if not bool(params) or params[0] == "":
        # TODO get this from config
        params = [config.prepare("REPL", "ENGINE")()]

    init_module = importlib.import_module(splitext(params[0])[0])
    # build engine
    engine = eval('init_module{}'.format(splitext(params[0])[1]))()
    engine.build_DSL()
    self.state.engine = engine

@register
def do_module(self, line):
    """
    Load an acab compliant python module into the self
    Rebuilds the DSL after import.
    """
    # TODO split this
    params = line
    logging.info("Loading Module: {}".format(params))
    self.state.engine.load_modules(*params)

@register
def do_load(self, line):
    """
    Load a dsl file into the self
    """
    params = line
    logging.info("Loading: {}".format(params))
    filename = abspath(expanduser(params[0])).strip()
    assert(exists(filename)), filename
    self.state.engine.load_file(filename)


@register
def do_save(self, line):
    """
    Save the state of the working memory into a file
    """
    params = line
    logging.info("Saving State to: {}".format(params))
    filename = abspath(expanduser(params[0]))
    assert(exists(split(filename)[0]))
    self.state.engine.save_file(filename)

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
def do_run(self, line):
    """
    Take a binding from a query, and run it.
    Used for running rules, queries, actions, layers, pipelines...
    """
    params = line
    result = None
    logging.info("Running: {}".format(params))
    # query
    if not bool(params):
        result = self.state.engine.tick()
        self.state.result = result
        return

    query_result = self.state.engine.query(params[-1])
    assert(len(query_result) == 1)

    # Get var
    value = query_result[0][params[-2][1]]

    result = self.state.engine(value)

    self.state.result = result


@register
def do_pass(self, line):
    """
    Pass strings through to the self/working memory
    """
    params = line
    logging.info("Passing sentences through: {}".format(params))
    # Determine query / assertion
    # FIXME: Simple hack for now
    if not bool(params[0]):
        result = ""
    elif params[0].strip()[-1] == "?":
        result = self.state.engine.query(*params)
    else:
        result = self.state.engine.add(*params)

    self.state.result = result


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
def do_step(self, line):
    """
    Control the engine state,
    Enabling rewinding to last saved rng position (ie: start of this pipeline loop)
    and stepping forwards by rule/layer/pipeline
    """
    params = line
    logging.info("Stepping {}".format(params))
    # TODO
    result = []
    if "back" in params[0]:
        print("back")
    elif "rule" in params[0]:
        print("rule")
    elif "layer" in params[0]:
        print("layer")
    elif "pipeline" in params[0]:
        print("pipeline")

    self.state.result = "\n".join(result)


@register
def do_act(self, line):
    """
    Manually perform an output action
    """
    params = line
    logging.info("Manually acting: {}".format(params))
    # TODO
    result = []
    # Parse the act / retrieve the act

    # combine with a binding

    # call act

    self.state.result = "\n".join(result)


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
def do_prompt(self, line):
    """
    Change the prompt of the repl
    """
    self.prompt = line



@register
def do_exit(self, line):
    """
    Exit the repl, automatically saving the self state
    """
    logging.info("Quit Command")
    filename = "{}_{}.auto".format(self.__class__.__name__,
                                   datetime.now().strftime("%Y_%m-%d_%H_%M"))
    self.state.engine.save_file(filename)
    return True


@register
def do_multi(self, line):
    """
    Activate multi-line collation
    """
    param = line
    if not self.state.in_multi_line:
        # Start
        logging.info("Activating multi line")
        self.state.in_multi_line = True
        self.state.prompt_bkup = self.prompt
        self.prompt = self.state.prompt_ml
    else:
        logging.info("Deactivating multi line")
        self.state.in_multi_line = False
        self.state.params = ["\n".join(self.state.collect_str)]
        self.state.collect_str = []
        self.state.current_str = ""
        self.prompt = self.state.prompt_bkup



@register
def do_multi_pop(self, line):
    """
    Pop off the last string added in multi-line mode,
    for when an error was made
    """
    self.state.collect_str.pop()


@register
def do_nop(self, line):
    """
    A null command used to collect strings in multi-line mode
    without making any changes to the self state
    """
    if self.state.in_multi_line:
        self.state.collect_str.append(line)
        logging.info("Collecting: {}".format(self.state.collect_str))



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
