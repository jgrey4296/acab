# https://docs.python.org/3/library/cmd.html
import cmd
import logging as root_logger
import sys
import pyparsing as pp
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
import traceback

logging = root_logger.getLogger(__name__)

import acab

config = acab.setup()

from acab.abstract.interfaces.context import ContextContainer_i
from acab.abstract.interfaces.engine import AcabEngine_i
from acab.modules.repl import ReplParser as RP


def register(fn):
    """ Decorator for registering a function into the repl """
    logging.info(f"Repl Registration: {fn.__name__}")
    assert("do_" in fn.__name__)
    assert(fn.__name__ not in dir(AcabREPLCommander))
    setattr(AcabREPLCommander, fn.__name__, fn)
    # Don't return fn as its only used in the class

#--------------------
initial_prompt = config.prepare("Module.REPL", "PROMPT", actions=[config.actions_e.STRIPQUOTE])()
initial_engine = config.prepare("Module.REPL", "ENGINE")()

@dataclass
class ReplState:
    """ Data used for control of the repl """
    prompt           : str                    =  field(default=initial_prompt)
    prompt_ml        : str                    =  field(default=config.prepare("Module.REPL", "PROMPT_ML", actions=[config.actions_e.STRIPQUOTE])())
    prompt_bkup      : str                    =  field(default="")
    result           : ContextContainer_i     =  field(default=None)
    collect_str      : List[str]              =  field(default_factory=list)
    echo             : bool                   =  field(default=False)
    in_multi_line    : bool                   =  field(default=False)
    engine           : Optional[AcabEngine_i] =  field(default=None)
    engine_str       : str                    =  field(default=initial_engine)
    debug_data       : Any                    =  field(default=None)


class AcabREPLCommander(cmd.Cmd):
    """ Implementation of cmd.Cmd to provide an extensible ACAB REPL"""
    intro  = "Welcome to ACAB. Type 'help' or '?' to list commands.\n"
    prompt = initial_prompt + ": "

    state  : ReplState = ReplState()

    def default(self, line):
        """ Called when no other command matches """
        try:
            # default to assertion / query / run
            self.state.result = self.state.engine(line,
                                            bindings=self.state.result)

        except pp.ParseException as err:
            logging.warning(f"Parse Failure: {err.markInputline()}")
        except Exception as err:
            traceback.print_tb(err.__traceback__)
            logging.warning(f"Failure in Default: {err}")


    def precmd(self, line):
        """ For massaging the input command """
        # convert symbols -> cmd names.
        # eg: ':{' -> multi
        try:
            line = RP.precmd_parser.parseString(line)[:]

            # Intercept if in multi line state
            if self.state.in_multi_line and not line[0] in ["multi", "pop"]:
                logging.info("In Multi")
                line = ["collect"] + line

            if bool(self.state.echo):
                print(f"{line}")

            return " ".join(line)

        except pp.ParseException as err:
            traceback.print_tb(err.__traceback__)
            logging.warning(f"Parse Failure: {err.markInputline()}")

    def postcmd(self, stop, line):
        count = "0"
        if self.state.result is not None:
            count = len(self.state.result)
        insert = f"(Γ: {count})"

        self.prompt = self.state.prompt + " " + insert + ": "
        return stop
