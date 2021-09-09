#!/usr/bin/env python3
import logging as root_logger
logging = root_logger.getLogger(__name__)

from acab.modules.semantics.context_container import ContextContainer

def MaybeBuildOperatorCtxDecorator(method):
    """ Wrap an Engine method to:
    Force a ContextContainer be passed to semantics,
    and ensure that container is loaded with operators from loaded modules
    """
    def fn(self, *args, **kwargs):
        no_bindings = 'bindings' not in kwargs or kwargs['bindings'] is None
        cached_ops  = self.semantics.has_op_cache
        if no_bindings and not cached_ops:
            logging.info("Building CtxContainer from modules")
            mods               = list(self._module_loader.loaded_modules.values())
            ctxcon             = self.semantics.build_ctxcon(mods)
            kwargs['bindings'] = ctxcon
        elif no_bindings and cached_ops:
            logging.info("Building CtxContainer from cached operators")
            ctxcon             = self.semantics.build_ctxcon()
            kwargs['bindings'] = ctxcon
        else:
            assert(not no_bindings)

        return method(self, *args, **kwargs)

    fn.__name__ = method.__name__
    return fn
