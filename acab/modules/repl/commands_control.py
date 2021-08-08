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
config = acab.setup()

from acab.modules.repl.repl_cmd import register
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionStructure

logging = root_logger.getLogger(__name__)

@register
def do_prompt(self, line):
    """
    Change the prompt of the repl
    """
    self.prompt = line.strip() + " "

@register
def do_multi(self, line):
    """
    Activate multi-line collation
    """
    if not self.state.in_multi_line:
        # Start
        logging.info("Activating multi line")
        self.state.in_multi_line = True
        self.state.prompt_bkup = self.prompt
        self.prompt = self.state.prompt_ml
    else:
        logging.info("Deactivating multi line")
        collected = "\n".join(self.state.collect_str)
        self.state.in_multi_line = False
        self.prompt = self.state.prompt_bkup
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
