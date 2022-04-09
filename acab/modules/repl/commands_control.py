"""
Commands for the REPL

"""
import importlib
import logging as logmod
import re
import traceback
from datetime import datetime
from enum import Enum
from os.path import abspath, exists, expanduser, split, splitext

import acab
import pyparsing as pp

config = acab.GET()

from acab.core.data.instruction import (ProductionOperator,
                                        ProductionStructure)
from acab.interfaces.value import Instruction_i
from acab.interfaces.context import ContextSet_i
from acab.modules.repl import ReplParser as RP
from acab.modules.repl.repl_commander import register
from acab.modules.repl.util import ConfigBasedLoad

logging = logmod.getLogger(__name__)


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
        if bool(line):
            self.onecmd(line + " " + collected)
        else:
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
def do_ctx(self, line):
    """ Control contexts.
    Select a subset using a slice '[1:-2]'
    Clear the context with '-'
    """
    try:
        params = RP.ctx_select_parser.parse_string(line)

        if "subset" in params:
            result = self.state.ctxs[params.subset]
            if not isinstance(result, ContextSet_i):
                self.state.ctxs = self.state.ctxs.subctx([result])

        elif "clear" in params:
            print("Clearing Context")
            self.state.ctxs = None
        else:
            self.onecmd("print_ctx")

    except pp.ParseException as err:
        logging.error(f"Failed to select context: {err.markInputline()}")



@register
def do_suppress(self, line):
    """
    Repl Command to suppress all repl commands
    (except itself and `exit`.)
    """
    # TODO
    return
