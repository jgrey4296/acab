#!/usr/bin/env python3

from enum import Enum

from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import AcabValue

import logging as root_logger
logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

def RunInSubCtxSet(f):
    """ Used to easily wrap around rules, to provide
    an isolated context set for execution
    """
    def wrapped(self, *the_args, **the_kwargs):
        semSys = the_args[1]
        ctxs   = the_kwargs['ctxs']
        temp_container = semSys.build_ctxset(ops=ctxs._operators)
        temp_container.set_parent(ctxs)
        # register the subctx for merging:
        ctxs.delay(ctxs.delayed_e.MERGE, temp_container)
        the_kwargs['ctxs'] = temp_container
        return f(self, *the_args, **the_kwargs)

    wrapped.__name__ = f.__name__
    return wrapped

def OperatorArgUnWrap(f):
    """ Use to simplify extracting raw values for use in operators,
    and wrapping the results into AcabValues

    Like a Monad, it extracts values from the Acab system,
    allows the operator to run on non-acab values (ints, np.matrix, strings, etc)
    then lifts the result back up for Acab to continue using
    """
    def wrapped(self, *the_args, **the_kwargs):
        unwrapped_args = [x.value for x in the_args]
        return f(self, *unwrapped_args, **the_kwargs)

    wrapped.__name__ = f"ArgUnwrap({f})"
    return wrapped

def OperatorDataUnWrap(f):
    """ Use to simplify extracting raw values for use in operators,
    and wrapping the results into AcabValues """
    def wrapped(self, *the_args, **the_kwargs):
        if 'data' in the_kwargs:
            unwrapped_data = {x: y.value for x,y in the_kwargs['data'].items()}
            the_kwargs['data'] = unwrapped_data
        return f(self, *the_args, **the_kwargs)

    wrapped.__name__ = f"DataUnwrap({f})"
    return wrapped

def OperatorResultWrap(f):
    def wrapped(self, *the_args, **the_kwargs):
        return AcabValue.safe_make(result)

    wrapped.__name__ = f"ResultWrap({f})"
