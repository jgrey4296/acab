#!/usr/bin/env python3

from enum import Enum

from acab import types as AT
from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import AcabValue
from acab.abstract.core.production_abstractions import ProductionOperator
from acab.abstract.interfaces.context import DelayedCommands_i

import logging as root_logger
logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

def BuildCtxSetIfMissing(f):
    """ Utility to Build a default CtxSet if one isnt provided """
    def wrapped(self, *the_args, **the_kwargs):
        if 'ctxs' not in the_kwargs or the_kwargs['ctxs'] is None:
            the_kwargs['ctxs'] = self.build_ctxset()

        return f(self, *the_args, **the_kwargs)

    wrapped.__name__ = f.__name__
    return wrapped

def RunDelayedCtxSetActions(f):
    """ Utility to run delayed ContextSet update actions """
    def wrapped(self, *the_args, **the_kwargs):
        result = f(self, *the_args, **the_kwargs)
        if isinstance(result, DelayedCommands_i):
            result.run_delayed()

        logging.debug(f"Returning CtxSet: {repr(the_kwargs['ctxs'])}")
        return result

    wrapped.__name__ = f.__name__
    return wrapped


def RunInSubCtxSet(f):
    """ Used to easily wrap around rules, to provide
    an isolated context set for execution
    """
    def wrapped(self, *the_args, **the_kwargs):
        semSys = the_args[1]
        ctxs   = the_kwargs['ctxs']
        subctx = ctxs.subctx()
        # register the subctx for merging:
        ctxs.delay(ctxs.delayed_e.MERGE, subctx)
        the_kwargs['ctxs'] = subctx
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
        return AcabValue.safe_make(f(self, *the_args, **the_kwargs))

    wrapped.__name__ = f"ResultWrap({f})"
    return wrapped


def OperatorSugar(sugar:str, prefix=None):
    """
    Decorates a ProductionOperator to carry a syntactic sugar annotation
    for semantic recognition.
    Stores in pseudo-sentence form: _:{sugar}
    """
    def wrapped(cls:ProductionOperator):
        psugar : AT.pseudo = "_:"
        if prefix is not None:
            psugar += prefix
            psugar += "."
        psugar += sugar

        cls._acab_operator_sugar = psugar
        return cls

    return wrapped
