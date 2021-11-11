"""
The Typing Submodule Provides a TypeChecker,

"""
from acab.core.config.config import AcabConfig

if AcabConfig.instance is not None:
    from .type_printer import *
    from .typing_dsl import *
