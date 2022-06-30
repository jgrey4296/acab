#!/usr/bin/env python3
from __future__ import annotations

import logging as logmod
from functools import wraps

from acab.error.parse import AcabParseException

logging = logmod.getLogger(__name__)

def MaybeBuildOperatorCtx(method):
    """ Wrap an Engine method to:
    Force a ContextSet be passed to semantics,
    and ensure that container is loaded with operators from loaded modules
    """
    @wraps(method)
    def build_operator_ctx_if_missing(self, *args, **kwargs):
        no_ctxset = 'ctxset' not in kwargs or kwargs['ctxset'] is None or not bool(kwargs['ctxset'])
        cached_ops  = self.semantics.has_op_cache
        if no_ctxset and not cached_ops:
            logging.info("Building Operator CtxSet from modules")
            mods             = self._module_loader.loaded
            ctxset           = self.semantics.build_ctxset(mods)
            kwargs['ctxset'] = ctxset
        elif no_ctxset and cached_ops:
            ctxset           = self.semantics.build_ctxset()
            kwargs['ctxset'] = ctxset
        else:
            assert(not no_ctxset)

        return method(self, *args, **kwargs)

    return build_operator_ctx_if_missing


def EnsureEngineInitialised(method):
    """ Utility Decorator to raise an error if the DSL hasn't been initialised """
    @wraps(method)
    def engine_must_be_initialised(self, *args, **kwargs):
        if not self.initialised:
            raise AcabParseException("Engine Not Initialised")

        return method(self, *args, **kwargs)

    return engine_must_be_initialised
