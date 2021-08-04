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

from repl_cmd import register
from acab.abstract.core.production_abstractions import ProductionOperator, ProductionStructure

logging = root_logger.getLogger(__name__)

@register
def do_prompt(self, line):
    """
    Change the prompt of the repl
    """
    self.prompt = line



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



