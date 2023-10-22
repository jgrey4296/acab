"""

reminder: https://docs.python.org/3/library/cmd.html
"""
##-- imports
from __future__ import annotations

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
from acab.error.base import AcabBasicException
from acab_config import AcabConfigException
from acab.error.parse import AcabParseException
from acab.interfaces.context import ContextSet_i
from acab.interfaces.engine import AcabEngine_i
from acab.modules.repl import ReplParser as RP

from .repl_state import ReplState

##-- end imports

##-- logging
logging      = logmod.getLogger(__name__)
trace_logger = logmod.getLogger('acab.repl.trace')
##-- end logging

##-- config
config       = acab.config
initial_prompt = config.module.REPL.PROMPT
try:
    repl_intro     = config.module.REPL.intro
except AcabConfigException:
    repl_intro = ["Welcome to ACAB.", "Type 'help' or '?' to list commands.", "Type 'tutorial' for a tutorial.", "Type ':q' to quit."]

##-- end config

class AcabREPLCommander(cmd.Cmd):
    """ Implementation of cmd.Cmd to provide an extensible ACAB REPL"""
    intro                                                   = "\n".join(repl_intro)
    prompt                                                  = initial_prompt + ": "
    _latebind                                               = []
    _default_startups  : ClassVar[list[Callable[..., Any]]] = []

    state  : ReplState = ReplState()

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        for inst in self._latebind:
            assert(getattr(inst, "_repl") is None)
            setattr(inst, "_repl", self)

        for fn in self._default_startups:
            fn(self, "")

    def default(self, line):
        """ Called when no other command matches """
        # default to assertion / query / run
        try:
            self.state.ctxs = self.state.engine(line,
                                                ctxset=self.state.ctxs)
        except AcabParseException as err:
            print(str(err))
        except AcabBasicException as err:
            logging.warning("\n--------------------\nFailure:\n")
            traceback.print_tb(err.__traceback__)
            logging.warning(f"\n{err.args[-1]}\n")
            print(str(err))

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

    def onecmd(self, line):
        try:
            return super(AcabREPLCommander, self).onecmd(line)
        except Exception as err:
            traceback.print_tb(err.__traceback__)
            print(f"\n{err.args[-1]}\n")

    def postcmd(self, stop, line):
        """
        Update the repl prompt to display number of viable contexts
        """
        for name, hook in sorted(self.state.post_cmds.items()):
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
            if (not bool(target_cls.__call__.__doc__)) and bool(target_cls.__doc__):
                target_cls.__call__.__doc__ = target_cls.__doc__

            instance = target_cls()
            assert(not hasattr(target_cls, "_repl"))
            setattr(cls, f"do_{name}", instance.__call__)
            setattr(instance, "_repl", None)

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


##-- utils
register         = AcabREPLCommander.register
register_class   = AcabREPLCommander.register_class
register_default = AcabREPLCommander.register_default

##-- end utils
