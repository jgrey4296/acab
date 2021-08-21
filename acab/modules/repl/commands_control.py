"""
Commands for the REPL

"""
from datetime import datetime
from enum import Enum
from os.path import split, splitext, exists, expanduser, abspath
import importlib
import logging as root_logger
import re
import pyparsing as pp
import traceback

import acab
config = acab.setup()

from acab.modules.repl.repl_cmd import register
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionStructure
from acab.modules.repl import ReplParser as RP

logging = root_logger.getLogger(__name__)

@register
def do_prompt(self, line):
    """
    Change the prompt of the repl
    """
    self.state.prompt = line.strip()

@register
def do_multi(self, line):
    """
    Activate multi-line collation
    """
    if not self.state.in_multi_line:
        # Start
        logging.info("Activating multi line")
        self.state.in_multi_line = True
        self.state.collect_str = []
        self.state.prompt_bkup = self.state.prompt
        self.state.prompt = self.state.prompt_ml
    else:
        logging.info("Deactivating multi line")
        collected = "\n".join(self.state.collect_str)
        self.state.in_multi_line = False
        self.state.prompt = self.state.prompt_bkup
        logging.info(f"Collected: {collected}")
        self.onecmd(collected)


@register
def do_pop(self, line):
    """
    Pop off the last string added in multi-line mode,
    for when an error was made
    """
    self.state.collect_str.pop()
    logging.info(f"Collecting: {self.state.collect_str}")

@register
def do_collect(self, line):
    """ Add a line to the multi line collection,
    ready to be used as one statement when multi line is closed """
    assert(self.state.in_multi_line)
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
    print("""
    Shunting to Python.
    Explore using: self.state, self.state.engine
    self is the repl,
    self.state is data the repl tracks,
    self.state.engine is the active ACAB engine
    """)
    breakpoint()


@register
def do_ctx(self, line):
    """ Control context selection """
    try:
        params = RP.ctx_select_parser.parseString(line)

        if "subset" in params:
            self.state.result = self.state.result.__getitem__(params.subset, wrap=True)
        elif "clear" in params:
            print("Clearing Context")
            self.state.result = None
        else:
            self.onecmd("print_ctx")

    except pp.ParseException as err:
        logging.error(f"Failed to select context: {err.markInputline()}")


@register
def do_forcep(self, line):
    """ Force Parser:
    Override built dsl, and call a parser from the boostrapper directly
    """
    try:
        # parse the line
        params = RP.force_parser.parseString(line)
        # query the bootstrapper for the parser
        parser = self.state.engine._dsl_builder._bootstrap_parser.query(params.query)
        print(f"Retrieved: {parser}\n")
        if not bool(params.send):
            print("Nothing sent to parser")
            return

        # if exists, parse, then call engine on it
        forced_result = parser.parseString(params.send)[0]
        self.state.debug_data = forced_result
        print(f"Forced Parse: {forced_result}\n")

        if isinstance(forced_result, tuple):
            forced_result = forced_result[1]

        self.state.result = self.state.engine(forced_result,
                                        bindings=self.state.result)

    except pp.ParseException as err:
        logging.warning(f"Parse Failure: {err.markInputline()}")
    except Exception as err:
        print("\n")
        traceback.print_tb(err.__traceback__)
        print("\n")
        logging.warning(f"Force Failure: {err}")
