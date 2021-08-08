# https://docs.python.org/3/library/cmd.html
import cmd
import logging as root_logger
import sys
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

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
    assert(fn.__name__ not in dir(AcabREPL))
    setattr(AcabREPL, fn.__name__, fn)
    # Don't return fn as its only used in the class

#--------------------
initial_prompt = config.prepare("Module.REPL", "PROMPT", actions=[config.actions_e.STRIPQUOTE])()
initial_engine = config.prepare("Module.REPL", "ENGINE")()

@dataclass
class ReplState:
    """ Data used for control of the repl """
    prompt        : str                    =  field(default=initial_prompt)
    prompt_ml     : str                    =  field(default=config.prepare("Module.REPL", "PROMPT_ML", actions=[config.actions_e.STRIPQUOTE])())
    result        : ContextContainer_i     =  field(default=None)
    collect_str   : List[str]              =  field(default_factory=list)
    echo          : bool                   =  field(default=False)
    in_multi_line : bool                   =  field(default=False)
    engine        : Optional[AcabEngine_i] =  field(default=None)
    engine_str    : str                    =  field(default=initial_engine)


class AcabREPL(cmd.Cmd):
    """ Implementation of cmd.Cmd to provide an extensible ACAB REPL"""
    intro  = "Welcome to ACAB. Type help to list commands.\n"
    prompt = initial_prompt

    state  : ReplState = ReplState()

    def default(self, line):
        """ Called when no other command matches """
        # default to assertion / query
        try:
            self.state.result = self.state.engine(line)
            logging.info(f"Result length: {len(self.state.result)}")
        except Exception as err:
            logging.warning(f"Failure in Default: {err}")


    def precmd(self, line):
        """ For massaging the input command """
        # convert symbols -> cmd names.
        # eg: ':{' -> multi
        line = RP.precmd_parser.parseString(line)[0]

        # Intercept if in multi line state
        if self.state.in_multi_line and not line in ["multi", "pop"]:
            line = f"collect {line}"

        if bool(self.state.echo):
            logging.info(f"{line}")

        return line
