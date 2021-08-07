from datetime import datetime
from enum import Enum
from os.path import split, splitext, exists, expanduser, abspath
import importlib
import logging as root_logger
import re

import acab
config = acab.setup()

from acab.modules.repl.repl_cmd import register
from acab.modules.repl import ReplParser as RP
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionStructure

logging = root_logger.getLogger(__name__)

@register
def do_init(self, line):
    """
    Specify the Engine to initialise.
    Imports the module, and uses the final component as the Engine Class.
    eg: acab.engines.trie_engine.TrieEngine -> TrieEngine
    """
    logging.info("Initialising Engine: {}".format(line))
    if not bool(line.strip()):
        line = self.state.engine_str

    try:
        mod = importlib.import_module(splitext(line[0])[0])
        # TODO ask for confirmation?
        # Note: not init_module.{} because of split*ext*
        # build engine. needs to be a 0 arg constructor
        engine_constructor = getattr(mod, line.split(".")[-1])
        self.state.engine = engine_constructor()
    except Exception as err:
        logging.error(f"Failed to initialise engine: {line}", exc_info=err)


@register
def do_module(self, line):
    """
    Load an acab compliant python module into the self
    Rebuilds the DSL after import.
    """
    params = line.split(" ")
    logging.info(f"Loading Modules: {params}")
    try:
        self.state.engine.load_modules(*params)
    except Exception as err:
        logging.error(f"{err}")
        logging.error(f"Failed to load modules: {line}")

@register
def do_load(self, line):
    """
    Load a dsl file into the self
    """
    logging.info(f"Loading: {line}")
    filename = abspath(expanduser(params[0])).strip()
    try:
        assert(exists(filename)), filename
        self.state.engine.load_file(filename)
    except Exception as err:
        logging.error(f"Failed to load: {line}")
        logging.error(f"{err}")


@register
def do_save(self, line):
    """
    Save the state of the working memory into a file
    """
    logging.info(f"Saving State to: {line}")
    filename = abspath(expanduser(params[0]))
    try:
        assert(exists(split(filename)[0]))
        self.state.engine.save_file(filename)
     except Exception as err:
        logging.error(f"Failed to save: {line}")
        logging.error(f"{err}")


@register
def do_run(self, line):
    """
    Take a binding from a query, and run it.
    Used for running rules, queries, actions, layers, pipelines...
    """
    result = None
    # query
    if not bool(line.strip()):
        logging.info("TODO Ticking Engine")
        # result = self.state.engine.tick()
        # self.state.result = result
        return

    logging.info("Running: {line}")
    try:
        query_result = self.state.engine.query(params[-1])
        assert(len(query_result) == 1)

        # Get var
        value = query_result[0][params[-2][1]]
        self.state.result = self.state.engine(value)
    except Exception as err:
        logging.error(f"Failed to run: {line}")
        logging.error(f"{err}")



@register
def do_step(self, line):
    """
    Control the engine state,
    Enabling rewinding to last saved rng position (ie: start of this pipeline loop)
    and stepping forwards by rule/layer/pipeline
    """
    logging.info(f"Stepping {line}")
    params = RP.step_parser.parseString(line)[:]
    # TODO
    try:
        result = []
        if "back" in params[0]:
            print("back")
        elif "rule" in params[0]:
            print(f"rule: {params[1]}")
        elif "layer" in params[0]:
            print("layer")
        elif "pipeline" in params[0]:
            print("pipeline")

        self.state.result = "\n".join(result)
    except Exception as err:
        logging.error(f"Failed to step: {line}")
        logging.error(f"{err}")



@register
def do_act(self, line):
    """
    Manually perform an output action
    """
    logging.info(f"Manually acting: {line}")
    # TODO
    try:
        # Parse / retrieve the act

        # combine with a binding?

        # call act

        self.state.result = "\n".join(result)
    except Exception as err:
        logging.error(f"Failed to load: {line}")
        logging.error(f"{err}")



@register
def do_exit(self, line):
    """
    Exit the repl, automatically saving the self state
    """
    logging.info("Quitting")
    filename = "{}_{}.auto".format(self.__class__.__name__,
                                   datetime.now().strftime("%Y_%m-%d_%H_%M"))
    self.state.engine.save_file(filename)
    return True
