from datetime import datetime
from enum import Enum
from os.path import split, splitext, exists, expanduser, abspath
import importlib
import logging as root_logger
import re

import acab
config = acab.setup()

from acab.abstract.interfaces.engine import AcabEngine_i
from acab.error.acab_config_exception import AcabConfigException
from acab.modules.repl.repl_commander import register
from acab.modules.repl import ReplParser as RP


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
def do_run(self, line):
    """
    Take a binding from a query, and run it.
    Used for running rules, queries, actions, layers, pipelines...
    """
    # TODO
    result = None
    # query
    if not bool(line.strip()):
        logging.info("TODO Ticking Engine")
        self.state.result = self.state.engine.tick()
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
