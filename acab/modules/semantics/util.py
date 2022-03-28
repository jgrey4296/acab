#!/usr/bin/env python3

import logging as root_logger
from enum import Enum

from acab.core.config.config import AcabConfig
from acab.core.data.value import AcabValue

logging = root_logger.getLogger(__name__)

config = AcabConfig()
