#!/usr/bin/env python3
##-- imports
from __future__ import annotations

import logging as logmod

import acab

##-- end imports

logging = logmod.getLogger(__name__)

config  = acab.config

# Destructor Debugging ########################################################
DEBUG_DEL      = config.logging.DEL

def _debug_del(self):
    """ A Simple debugger del implementation to """
    logging.warning("Deleting: {}", self)

def _debug_del_dec(fn):
    """ A decorator for existing del methods """
    def _wrapped(*args):
        logging.warning("Deleting: {}", self)
        fn(*args)

def logdel(cls):
    """
    A Class Decorator, attaches a debugging statement to the object destructor
    """
    match (DEBUG_DEL, hasattr(cls, "__del__")):
        case (False, _):
            pass
        case (True, True):
            setattr(cls, "__del__", _debug_del_dec(cls.__del__))
        case (True, False):
            setattr(cls, "__del__", _debug_del)
    return cls



# TraceMalloc Printing ########################################################
import re
import tracemalloc
import linecache

human     = lambda x, y=False: tracemalloc._format_size(x, y)
REJECT    = False
ADMIT     = True
sh_filter = (
    tracemalloc.Filter(ADMIT, "*/acab/*"),
    tracemalloc.Filter(REJECT, "<frozen importlib._bootstrap>"),
    tracemalloc.Filter(REJECT, "<unknown>"),
    tracemalloc.Filter(REJECT, "*/site-packages/pyparsing/*.py"),
    tracemalloc.Filter(REJECT, "*/linecache.py"),
    tracemalloc.Filter(REJECT, "*/tracemalloc.py"),
    )

path_re = re.compile("^.+?(acab.+?)$")

def print_stat_file(stat):
    """
    Print a memory trace File Statistic
    """
    assert(isinstance(stat, (tracemalloc.Statistic, tracemalloc.StatisticDiff)))
    tb = stat.traceback
    frame  = tb[0]
    fname  = frame.filename
    fmatch = path_re.search(frame.filename)
    if fmatch:
        fname = fmatch[1]
    print(f"Count: {stat.count:<5}, Size: {human(stat.size):<10} : ",
          f"File: ...{fname[-23:]:>25}:{frame.lineno:<10}")

def print_stat(stat):
    """
    Print a memory trace stack
    """
    assert(isinstance(stat, (tracemalloc.Statistic, tracemalloc.StatisticDiff)))
    tb = stat.traceback
    print(f"Frame/Count/Size: {len(stat.traceback):}, {stat.count:}, {human(stat.size)}")
    for x in range(min(20, len(tb))):
        frame  = tb[x]
        fname  = frame.filename
        fmatch = path_re.search(frame.filename)
        if fmatch:
            fname = fmatch[1]
        print(f"{'File:':<6} ...{fname[-23:]:>25};{frame.lineno:<10} : ",
              f"{linecache.getline(frame.filename, frame.lineno).strip()}")

    print()


def print_diff(diff):
    """
    Print a Tracemalloc difference between two snapshots
    """
    assert(isinstance(diff, tracemalloc.StatisticDiff))
    tb = diff.traceback
    frame  = tb[0]
    fname  = frame.filename
    fmatch = path_re.search(frame.filename)
    if fmatch:
        fname = fmatch[1]
    print(f"{human(diff.size):<10} ({human(diff.size_diff, True):<10}, {diff.count_diff:<5})",
          f"{'File:':<6} ...{fname[-23:]:>25};{frame.lineno:<10} : ",
          f"{linecache.getline(frame.filename, frame.lineno).strip()}")
