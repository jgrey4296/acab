#!/usr/bin/env python3

from enum import Enum

from acab.abstract.config.config import AcabConfig

import logging as root_logger
logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()

# TODO RDFSemantics, ReteSemantics
