#!/usr/bin/env python3
import logging as root_logger
logging = root_logger.getLogger(__name__)

from acab.modules.semantics.context_set import ContextSet

def MaybeBuildOperatorCtxDecorator(method):
    """ Wrap an Engine method to:
    Force a ContextSet be passed to semantics,
    and ensure that container is loaded with operators from loaded modules
    """
    def fn(self, *args, **kwargs):
        no_bindings = 'bindings' not in kwargs or kwargs['bindings'] is None
        cached_ops  = self.semantics.has_op_cache
        if no_bindings and not cached_ops:
            logging.info("Building CtxSet from modules")
            mods               = list(self._module_loader.loaded_modules.values())
            ctxset             = self.semantics.build_ctxset(mods)
            kwargs['bindings'] = ctxset
        elif no_bindings and cached_ops:
            logging.info("Building CtxSet from cached operators")
            ctxset             = self.semantics.build_ctxset()
            kwargs['bindings'] = ctxset
        else:
            assert(not no_bindings)

        return method(self, *args, **kwargs)

    fn.__name__ = method.__name__
    return fn
