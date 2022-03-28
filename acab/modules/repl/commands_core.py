from datetime import datetime
from enum import Enum
from os.path import split, splitext, exists, expanduser, abspath
import importlib
import logging as logmod
import re
import traceback

import acab
config = acab.GET()

from acab.interfaces.engine import AcabEngine_i
from acab.interfaces.value import Instruction_i
from acab.error.config import AcabConfigException
from acab.error.importer import AcabImportException
from acab.modules.repl.repl_commander import register
from acab.modules.repl import ReplParser as RP
from acab.core.data.instruction import ProductionContainer
from acab.modules.repl.util import init_inspect
from acab.error.semantic import AcabSemanticException

logging = logmod.getLogger(__name__)

@register
def do_init(self, line):
    """
    Specify the Engine to initialise.
    Imports the module, and uses the final component as the Engine Class.
    eg: acab.modules.engines.trie_engine.TrieEngine -> TrieEngine

    A Question mark at the end of the line signals to inspect the module
    for potential constructors:
    eg: acab.modules.engines.configured?
    """
    if not bool(line.strip()):
        line = self.state.engine_str

    # if:  "init line?", then return applicable functions
    if line[-1] == "?":
        return init_inspect(line[:-1])


    logging.info("Initialising Engine: {}".format(line))

    try:
        mod_str = splitext(line)[0]
        mod = importlib.import_module(mod_str)
        # TODO ask for confirmation?
        # Note: not init_module.{} because of split*ext*
        # build engine. needs to be a 0 arg constructor
        spec        = getattr(mod, line.split(".")[-1])
        is_type     = isinstance(spec, type)
        is_sub      = is_type and issubclass(spec, AcabEngine_i)
        is_callable = callable(spec)
        if (not is_type) and isinstance(spec, AcabEngine_i):
            self.state.engine = spec
        elif (is_type and is_sub) or callable(spec):
            self.state.engine = spec()
        else:
            raise AcabConfigException(f"Unknown Engine Spec Form: {spec}")

        # TODO add bad words from repl:
        # self.state.engine.parser.set_word_exclusions(self.completenames(""))

        self.state.ctxs = None
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
        loaded = self.state.engine.load_modules(*params)
        print(f"Loaded {len(loaded)} modules")
        for x in loaded:
            print(x)

    except AcabImportException as err:
        traceback.print_tb(err.__traceback__, limit=-4)
        logging.error(f"Module Load: {err}")
        self.state.last_err = err

@register
def do_load(self, line):
    """
    Load a dsl file into the self
    TODO specify a prefix for everything from loaded
    """
    logging.info(f"Loading: {line}")
    filename = abspath(expanduser(line)).strip()
    self.state.engine.load_file(filename)

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

    run                : tick the engine
    run a.rule?        : runs a unique runnable at position
    run a.rule.$x?     : runs all matching runnables bound to $x
    run $x?            : run $x from active ctxs
    run override X SEN : run SEN with override semantics X
    """
    try:
        logging.info("------ Run Setup:")
        query_results = self.state.engine(line)

        bindings = [y for x in query_results.active_list()
                    for y in x if isinstance(y, ProductionContainer)]

        if not bool(bindings) and bool(query_results) and isinstance(query_results[0]._current.value, Instruction_i):
            bindings = [query_results[0]._current.value]
        logging.info("------ Run Setup End")

        if bool(bindings):
            # Run the bindings
            print(f"Running {len(bindings)} Statements")
            self.state.ctxs = self.state.engine(bindings, ctxset=self.state.ctxs)
        else:
            print("No Match to Run")

    except AcabSemanticException as err:
        traceback.print_tb(err.__traceback__)
        logging.error(f"\nFailed to run: {line}")
        logging.error(f"{err}")

    except Exception as err:
        traceback.print_tb(err.__traceback__)
        logging.error(f"\nFailed to run: {line}")
        logging.error(f"{err}")
