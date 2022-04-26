#!/usr/bin/env python3

import logging as logmod
logging = logmod.getLogger(__name__)

from acab.core.util.log_formatter import AcabLogFormatter, AcabLogRecord

def log_config(self):
    if not self.attr.LOGGING.ACAB:
        return
    logging.debug("Setting up Acab Log Formatting")
    log_fmt = self.attr.LOGGING.FORMAT

    root_logger = logmod.getLogger()
    stream_handler = [x for x in root_logger.handlers if isinstance(x, logmod.StreamHandler)]
    if bool(stream_handler):
        stream_handler[0].setFormatter(AcabLogFormatter(log_fmt))

    if logmod.getLogRecordFactory() is not AcabLogRecord:
        AcabLogRecord.install()
