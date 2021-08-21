#!/usr/bin/env python3
import logging as root_logger
logging = root_logger.getLogger(__name__)

from acab.modules.semantics.context_container import ContextContainer

def MaybeBuildOperatorCtx(method):
    """ Wrap an Engine method to:
    Force a ContextContainer be passed to semantics,
    and ensure that container is loaded with operators from loaded modules
    """
    def fn(self, *args, **kwargs):
        if 'bindings' not in kwargs or kwargs['bindings'] is None:
            logging.info("Building CtxContainer with operators")
            mods = list(self._module_loader.loaded_modules.values())
            kwargs['bindings'] = ContextContainer.build(mods)

        return method(self, *args, **kwargs)

    fn.__name__ = method.__name__
    return fn
