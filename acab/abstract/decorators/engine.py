#!/usr/bin/env python3
import logging as root_logger
logging = root_logger.getLogger(__name__)

from acab.error.acab_parse_exception import AcabParseException

def MaybeBuildOperatorCtx(method):
    """ Wrap an Engine method to:
    Force a ContextSet be passed to semantics,
    and ensure that container is loaded with operators from loaded modules
    """
    def fn(self, *args, **kwargs):
        no_ctxset = 'ctxset' not in kwargs or kwargs['ctxset'] is None or not bool(kwargs['ctxset'])
        cached_ops  = self.semantics.has_op_cache
        if no_ctxset and not cached_ops:
            logging.info("Building CtxSet from modules")
            mods             = list(self._module_loader.loaded_modules.values())
            ctxset           = self.semantics.build_ctxset(mods)
            kwargs['ctxset'] = ctxset
        elif no_ctxset and cached_ops:
            logging.info("Building CtxSet from cached operators")
            ctxset           = self.semantics.build_ctxset()
            kwargs['ctxset'] = ctxset
        else:
            assert(not no_ctxset)

        return method(self, *args, **kwargs)

    fn.__name__ = method.__name__
    return fn


def EnsureEngineInitialised(method):
    """ Utility Decorator to raise an error if the DSL hasn't been initialised """
    def fn(self, *args, **kwargs):
        if not self.initialised:
            raise AcabParseException("DSL Not Initialised")

        return method(self, *args, **kwargs)

    fn.__name__ = method.__name__
    return fn
