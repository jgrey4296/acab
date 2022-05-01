# https://docs.python.org/3/library/cmd.html
import cmd
import logging as logmod
import sys
import traceback
from dataclasses import InitVar, dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab
import pyparsing as pp
from acab import AcabConfig
from acab.error.config import AcabConfigException
from acab.interfaces.context import ContextSet_i
from acab.interfaces.engine import AcabEngine_i
from acab.modules.repl import ReplParser as RP

logging      = logmod.getLogger(__name__)
trace_logger = logmod.getLogger('acab.repl.trace')
config       = AcabConfig()
#--------------------
initial_prompt = config.prepare("Module.REPL", "PROMPT", actions=[config.actions_e.STRIPQUOTE])()
initial_engine = config.prepare("Module.REPL", "ENGINE")()
try:
    repl_intro     = config.prepare("Module.REPL", "INTRO")().replace("\\n", "\n")
except AcabConfigException:
    repl_intro = "Welcome to ACAB.\nType 'help' or '?' to list commands.\nType 'tutorial' for a tutorial.\nType ':q' to quit."

@dataclass
class ReplState:
    """ Data used for control of the repl """
    prompt           : str                            =  field(default=initial_prompt)
    prompt_ml        : str                            =  field(default=config.prepare("Module.REPL", "PROMPT_ML", actions=[config.actions_e.STRIPQUOTE])())
    prompt_bkup      : str                            =  field(default="")
    ctxs             : ContextSet_i                   =  field(default=None)
    collect_str      : list[str]                      =  field(default_factory=list)
    echo             : bool                           =  field(default=False)
    in_multi_line    : bool                           =  field(default=False)
    engine           : None|AcabEngine_i              =  field(default=None)
    engine_str       : str                            =  field(default=initial_engine)
    debug_data       : Any                            =  field(default=None)
    debugger         : Any                            =  field(default=None)
    last_err         : Any                            =  field(default=None)
    post_cmds        : dict[str, Callable[..., None]] =  field(default_factory=dict)


class AcabREPLCommander(cmd.Cmd):
    """ Implementation of cmd.Cmd to provide an extensible ACAB REPL"""
    intro                                                   = repl_intro
    prompt                                                  = initial_prompt + ": "
    _latebind                                               = []
    _default_startups  : ClassVar[list[Callable[..., Any]]] = []

    state  : ReplState = ReplState()

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        for inst in self._latebind:
            assert(getattr(inst, "_cmd") is None)
            setattr(inst, "_cmd", self)

        for fn in self._default_startups:
            fn(self, "")

    def default(self, line):
        """ Called when no other command matches """
        # default to assertion / query / run
        self.state.ctxs = self.state.engine(line,
                                            ctxset=self.state.ctxs)
        # except Exception as err:
        #     traceback.print_tb(err.__traceback__)
        #     logging.warning(f"Failure in Default: {err}")


    def precmd(self, line):
        """ For massaging the input command """
        # convert symbols -> cmd names.
        # eg: ':{' -> multi
        trace_logger.info("[repl]>>> " + line)
        try:
            logging.debug("PreCmd Parsing: {}".format(line))
            line = RP.precmd_parser.parse_string(line)[:]
            logging.debug("PreCmd Result:{}".format(line))
            # Intercept if in multi line state
            if self.state.in_multi_line and not line[0] in ["multi", "pop", "exit", "echo"]:
                logging.info("In Multi")
                line = ["collect"] + line

            if bool(self.state.echo):
                print(f"{line}")

            return " ".join(line)

        except pp.ParseException as err:
            traceback.print_tb(err.__traceback__)
            logging.warning(f"Parse Failure: {err.markInputline()}")

    def postcmd(self, stop, line):
        """
        Update the repl prompt to display number of viable contexts
        """
        for hook in self.state.post_cmds.values():
            hook(self)

        return stop

    def parseline(self, line):
        """Parse the line into a command name and a string containing
        the arguments.  Returns a tuple containing (command, args, line).
        'command' and 'args' may be None if the line couldn't be parsed.
        """
        if not self.state.in_multi_line:
            line = line.strip()

        if not line:
            return None, None, line
        elif line[0] == '?':
            line = 'help ' + line[1:]
        elif line[0] == '!':
            if hasattr(self, 'do_shell'):
                line = 'shell ' + line[1:]
            else:
                return None, None, line

        # split into cmd and args
        i, n = 0, len(line)
        while i < n and line[i] in self.identchars: i = i+1
        cmd, arg = line[:i], line[i:]

        if not self.state.in_multi_line:
            arg = arg.strip()

        return cmd, arg, line

    def emptyline(self):
        """ Overrides default of 'repeat last command',
        and prints the working memory
        """
        return self.onecmd("print wm")


    @classmethod
    def register(cls, fn):
        """ Decorator for registering a function into the repl """
        logging.debug(f"{cls.__name__} Registration: {fn.__name__}")
        assert("do_" in fn.__name__)
        assert(fn.__name__ not in dir(cls))
        setattr(cls, fn.__name__, fn)
        return fn

    @classmethod
    def register_class(cls, name):
        """ Register an entire class as the command bound to do_{name},
        (specifically the class' __call__ method)
        """
        def __register(target_cls):
            assert(hasattr(target_cls, "__call__"))
            assert(hasattr(cls, "_latebind"))
            instance = target_cls()
            assert(not hasattr(target_cls, "_cmd"))
            setattr(cls, f"do_{name}", instance.__call__)
            setattr(instance, "_cmd", None)
            cls._latebind.append(instance)
            return target_cls

        return __register


    @classmethod
    def register_default(cls, fn):
        """
        Register and automatically call the function when REPLCommander is created.
        eg: register_default(do_ctxprompt) means the repl will show active context numbers
        from startup

        """
        assert(hasattr(cls, "_default_startups"))
        cls.register(fn)
        cls._default_startups.append(fn)

register         = AcabREPLCommander.register
register_class   = AcabREPLCommander.register_class
register_default = AcabREPLCommander.register_default
