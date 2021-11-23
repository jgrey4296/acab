#!/usr/bin/env python3

import logging as root_logger
from enum import Enum

from acab.core.config.config import AcabConfig
from acab.core.data.values import AcabValue

logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

# Stub decorator to override
SemanticBreakpointDecorator = lambda f: f

if "Module.Debug" in config:
    mod = config.prepare("Module.Debug", "IMPORT", actions=[config.actions_e.IMPORT])()
    decorator_name = config.prepare("Module.Debug", "BREAK_DECORATOR")()
    SemanticBreakpointDecorator = getattr(mod, decorator_name)
