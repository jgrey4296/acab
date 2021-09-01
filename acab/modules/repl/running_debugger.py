# adapted from https://stackoverflow.com/questions/50691169
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir
import pdb
import sys

class RunningDebugger(pdb.Pdb):
    def set_running_trace(self, frame=None):
        """Start debugging from frame.

        If frame is not specified, debugging starts from caller's frame.
        """
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
