"""
Cross-module utilities for the rule engines
"""
from enum import Enum

from acab.abstract.core.node import AcabNode
from acab.abstract.config.config import AcabConfig
from acab.modules.semantics import util as SU

import logging as root_logger
logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()

OPERATOR_S = util.value("Parse.Structure", "OPERATOR")
ROOT_S     = util.value("Data", "ROOT")

