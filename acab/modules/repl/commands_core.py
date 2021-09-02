from datetime import datetime
from enum import Enum
from os.path import split, splitext, exists, expanduser, abspath
import importlib
import logging as root_logger
import re
import traceback

import acab
config = acab.setup()

from acab.abstract.interfaces.engine import AcabEngine_i
from acab.abstract.interfaces.value import Statement_i
from acab.error.acab_config_exception import AcabConfigException
from acab.modules.repl.repl_commander import register
from acab.modules.repl import ReplParser as RP
from acab.abstract.core.production_abstractions import ProductionContainer

logging = root_logger.getLogger(__name__)

@register
def do_init(self, line):
    """
    Specify the Engine to initialise.
    Imports the module, and uses the final component as the Engine Class.
    eg: acab.engines.trie_engine.TrieEngine -> TrieEngine
    """
    if not bool(line.strip()):
        line = self.state.engine_str
    logging.info("Initialising Engine: {}".format(line))

    try:
        mod_str = splitext(line)[0]
        mod = importlib.import_module(mod_str)
        # TODO ask for confirmation?
        # Note: not init_module.{} because of split*ext*
        # build engine. needs to be a 0 arg constructor
        spec = getattr(mod, line.split(".")[-1])
        is_type = isinstance(spec, type)
        is_sub  = is_type and issubclass(spec, AcabEngine_i)
        is_callable = callable(spec)
        if (not is_type) and isinstance(spec, AcabEngine_i):
            self.state.engine = spec
        elif (is_type and is_sub) or callable(spec):
            self.state.engine = spec()
        else:
            raise AcabConfigException(f"Unknown Engine Spec Form: {spec}")

        logging.info("Engine Initialisation Complete")
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
    filename = abspath(expanduser(line)).strip()
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
    filename = abspath(expanduser(line))
    try:
        assert(exists(split(filename)[0]))
        self.state.engine.save_file(filename)
    except Exception as err:
        logging.error(f"Failed to save: {line}")
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


@register
def do_run(self, line):
    """
    Run an action/transform or binding.
    Take a binding from a query, and run it.
    Used for running rules, queries, actions, layers, pipelines...

    run               : tick the engine
    run a.rule?       : runs a unique rule
    run a.rule.$x?    : runs all matching rules
    """
    # TODO
    try:
        # query
        if not bool(line.strip()):
            logging.info("TODO Ticking Engine")
            self.state.result = self.state.engine.tick()
            return

        self.state.result = self.state.engine(line)

        bindings = [y for x in self.state.result.active_list()
                    for y in x if isinstance(y, ProductionContainer)]

        if not bool(bindings) and bool(self.state.result) and isinstance(self.state.result[0]._current.value, Statement_i):
            bindings = [self.state.result[0]._current.value]

        if bool(self.state.result):
            # Run the bindings
            self.state.result = self.state.engine(bindings)
        else:
            print("No Match to Run")

    except Exception as err:
        traceback.print_tb(err.__traceback__)
        logging.error(f"Failed to run: {line}")
        logging.error(f"{err}")
