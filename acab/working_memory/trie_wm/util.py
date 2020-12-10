"""
Cross-module utilities for the rule engines
"""
from enum import Enum

from acab.abstract.core.node import AcabNode
from acab.abstract.config.config import AcabConfig
from acab.modules.node_semantics import util as SU

import logging as root_logger
logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

OPERATOR_S = config.value("Parse.Structure", "OPERATOR")
ROOT_S     = config.value("Data", "ROOT")
