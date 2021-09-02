# adapted from https://stackoverflow.com/questions/50691169
import logging as root_logger
import sys
from bdb import Breakpoint
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = root_logger.getLogger(__name__)


from acab.abstract.interfaces.debugger import AcabDebugger_i

# TODO track semantic debugging in RunningDebugger

def SemanticBreakpointDecorator(f):

    def wrapped(self, *args, **kwargs):
        # TODO handle repeats
        if args[0].should_break:
            f_code = f.__code__
            db = RunningDebugger.Get()
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


class RunningDebugger(AcabDebugger_i):

    @staticmethod
    def Get():
        if RunningDebugger.singleton is None:
           RunningDebugger.singleton = RunningDebugger()

        return RunningDebugger.singleton

    def set_running_trace(self, frame=None):
        """ Start debugging from frame, without pausing execution. """
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
        if frame is None:
            frame = sys._getframe().f_back
        # - reset here.
        while frame:
            frame.f_trace = self.trace_dispatch
            self.botframe = frame
            frame = frame.f_back
        self.set_step()
        sys.settrace(self.trace_dispatch)
