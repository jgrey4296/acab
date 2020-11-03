#!/usr/bin/env python3

from enum import Enum

from acab.config import AcabConfig

import logging as root_logger
logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()

MODAL_NAME = util.value("Modal.Exclusion", "MODAL_NAME")
MODAL_ENUM = util.value("Modal.Exclusion", "MODAL_ENUMS")

EXOP = Enum(MODAL_NAME, MODAL_ENUM)
