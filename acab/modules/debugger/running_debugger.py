# adapted from https://stackoverflow.com/questions/50691169
import logging as logmod
import sys
from bdb import Breakpoint
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)
trace_logger = logmod.getLogger('acab.repl.trace')

from acab.interfaces.debugger import AcabDebugger_i
from acab.core.util.singletons import SingletonMeta

# TODO track semantic debugging in RunningDebugger
# TODO refactor this to be a handler registration
# TODO add acab specific do_ methods
def SemanticBreakpointDecorator(f):
    logging.info("Attaching Semantic Breakpoint")

    def wrapped(self, *args, **kwargs):
        # TODO handle repeats
        if args[0].should_break:
            f_code = f.__code__
            db = RunningDebugger()
            # Ensure trace function is set
            sys.settrace(db.trace_dispatch)
            if not db.get_break(f_code.co_filename, f_code.co_firstlineno+2):
                db.set_break(f_code.co_filename,
                             f_code.co_firstlineno+2,
                             True)
            else:
                bp = Breakpoint.bplist[f_code.co_filename,
                                       f_code.co_firstlineno+2][0]
                bp.enable()


        return f(self, *args, **kwargs)

    wrapped.__name__ = f.__name__
    return wrapped

class RunningDebugger(AcabDebugger_i, metaclass=SingletonMeta):

    def __init__(self):
        super().__init__()
        self.running = False

    def __bool__(self):
        return self.running

    def precmd(self, line):
        trace_logger.info("[db]>>> " + line)
        return line

    def do_help(self, *args):
        print("Acab Debugger Help")
        super().do_help(*args)


    def set_running_trace(self, frame=None):
        """ Start debugging from frame, without pausing execution.
        This is to allow setting a future breakpoint, without having
        to enter the debugger and exit again.
        """
        self.running = True
        if frame is None:
            frame = sys._getframe().f_back
        self.reset()
        while frame:
            frame.f_trace = self.trace_dispatch
            self.botframe = frame
            frame = frame.f_back
        self.set_continue()
        sys.settrace(self.trace_dispatch)

    def set_trace(self, frame=None):
        """Start debugging from frame.

        If frame is not specified, debugging starts from caller's frame.
        """
        self.running = True
        if frame is None:
            frame = sys._getframe().f_back
        # removed "reset" here.
        while frame:
            frame.f_trace = self.trace_dispatch
            self.botframe = frame
            frame = frame.f_back
        self.set_step()
        sys.settrace(self.trace_dispatch)
