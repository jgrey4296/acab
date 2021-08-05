# https://docs.python.org/3/library/cmd.html
from dataclasses import dataclass, field, InitVar
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
import cmd, sys
import logging as root_logger
logging = root_logger.getLogger(__name__)

import acab
config = acab.setup()

from acab.abstract.interfaces.engine import AcabEngine_i

def register(fn):
    """ Decorator for registering a function into the repl """
    logging.info(f"Registering: {fn.__name__}")
    assert("do_" in fn.__name__)
    assert(fn.__name__ not in dir(AcabREPL))
    setattr(AcabREPL, fn.__name__, fn)
    # Don't return fn as its only used in the class

#--------------------
initial_prompt = config.prepare("Module.REPL", "PROMPT", actions=[config.actions_e.STRIPQUOTE])()

@dataclass
class ReplState:
    """ Data used for control of the repl """
    prompt        : str                    =  field(default=initial_prompt)
    prompt_ml     : str                    =  field(default=config.prepare("Module.REPL", "PROMPT_ML", actions=[config.actions_e.STRIPQUOTE])())
    params        : List[str]              =  field(default_factory=list)
    result        : Any                    =  field(default=None)
    current_str   : str                    =  field(default=None)
    collect_str   : List[str]              =  field(default_factory=list)
    echo          : bool                   =  field(default=False)
    stack         : bool                   =  field(default=False)
    in_multi_line : bool                   =  field(default=False)
    engine        : Optional[AcabEngine_i] =  field(init=None)

class AcabREPL(cmd.Cmd):
    intro  = "Welcome to ACAB. Type help to list commands.\n"
    prompt = initial_prompt

    state  : ReplState = ReplState()

    def default(self, line):
        """ Called when no other command matches """
        logging.warning(f"No recognised command: {line}")
        if self.state.in_multi_line:
            self.state.collect_str.append(line)
            logging.info("Collecting: {}".format(self.state.collect_str))
        else:
            # TODO default to assertion / query
            # self.state.engine(line)
            pass




    def precmd(self, line):
        """ For massaging the input command """
        # TODO convert symbols -> cmd names.
        # eg: ':{' -> multi

        if self.state.in_multi_line and not line == "multi":
            return f"nop {line}"

        return line
