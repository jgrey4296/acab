#!/usr/bin/env python3

from functools import wraps
from enum import Enum

from acab import types as AT
from acab.core.config.config import AcabConfig
from acab.core.value.value import AcabValue
from acab.core.value.instruction import ProductionOperator
from acab.core.util.delayed_commands import DelayedCommands_i

import logging as logmod
logging = logmod.getLogger(__name__)

config = AcabConfig()

def BuildCtxSetIfMissing(f):
    """ Utility to Build a default CtxSet if one isnt provided """
    @wraps(f)
    def EnsureCtxSet(self, *the_args, **the_kwargs):
        if 'ctxs' not in the_kwargs or the_kwargs['ctxs'] is None:
            logging.debug("Building CtxSet")
            the_kwargs['ctxs'] = self.build_ctxset()

        return f(self, *the_args, **the_kwargs)

    return EnsureCtxSet

def RunDelayedCtxSetActions(f):
    """ Utility to run delayed ContextSet update actions """
    @wraps(f)
    def DelayedActionRunner(self, *the_args, **the_kwargs):
        result = f(self, *the_args, **the_kwargs)
        if isinstance(result, DelayedCommands_i):
            result.run_delayed()

        logging.debug("Returning CtxSet: {}", the_kwargs['ctxs'])
        return result

    return DelayedActionRunner


def RunInSubCtxSet(f):
    """ Used to easily wrap around rules, to provide
    an isolated context set for execution.
    # TODO move this decorator into handler?
    """
    @wraps(f)
    def RunningInSubCtx(self, *the_args, **the_kwargs):
        logging.debug("Creating Subctx")
        semSys = the_args[1]
        ctxs   = the_kwargs['ctxs']
        subctx = ctxs.subctx()
        # register the subctx for merging:
        ctxs.delay(ctxs.delayed_e.MERGE, val=subctx)
        the_kwargs['ctxs'] = subctx
        return f(self, *the_args, **the_kwargs)

    return RunningInSubCtx


def OperatorArgUnWrap(f):
    """ Use to simplify extracting raw values for use in operators,
    and wrapping the results into AcabValues

    Like a Monad, it extracts values from the Acab system,
    allows the operator to run on non-acab values (ints, np.matrix, strings, etc)
    then lifts the result back up for Acab to continue using
    """
    @wraps(f)
    def UnwrapArgsForOperator(self, *the_args, **the_kwargs):
        unwrapped_args = [x.value or x.key() for x in the_args]
        return f(self, *unwrapped_args, **the_kwargs)

    return UnwrapArgsForOperator

def OperatorDataUnWrap(f):
    """ Use to simplify extracting raw values for use in operators,
    and wrapping the results into AcabValues """
    @wraps(f)
    def UnwrapDataForOperator(self, *the_args, **the_kwargs):
        if 'data' in the_kwargs:
            unwrapped_data = {x: y.value for x,y in the_kwargs['data'].items()}
            the_kwargs['data'] = unwrapped_data
        return f(self, *the_args, **the_kwargs)

    return UnwrapDataForOperator

def OperatorResultWrap(f):
    @wraps(f)
    def WrapOperatorResultAsValue(self, *the_args, **the_kwargs):
        return AcabValue(f(self, *the_args, **the_kwargs))

    return WrapOperatorResultAsValue


def OperatorSugar(sugar:str, prefix=None):
    """
    Decorates a ProductionOperator to carry a syntactic sugar annotation
    for semantic recognition.
    """
    def AnnotateOperatorWithSugar(cls:ProductionOperator):
        psugar : str = "" #"_:"
        if prefix is not None:
            psugar += prefix
            psugar += "."
        psugar += sugar

        cls._acab_operator_sugar = psugar
        return cls

    return AnnotateOperatorWithSugar
