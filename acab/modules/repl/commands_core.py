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
def do_exit(self, line):
    """
    Exit the repl, automatically saving the self state
    """
    logging.info("Quit Command")
    filename = "{}_{}.auto".format(self.__class__.__name__,
                                   datetime.now().strftime("%Y_%m-%d_%H_%M"))
    self.state.engine.save_file(filename)
    return True
