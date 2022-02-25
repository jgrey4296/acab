#!/usr/bin/env python3

import logging as root_logger
import pyparsing as pp

from acab.core.parsing.consts import DOUBLEBAR, COLON, COMMA, DBLCOLON, DELIM, component_gap
from acab.core.parsing.consts import N, NG, op, OPAR, CPAR
from acab.core.data.value import Sentence
from acab.core.parsing import parsers as PU

from acab.modules.analysis.typing import util as TYU

from acab.core.config.config import AcabConfig

HOTLOAD_SENTENCE = pp.Forward()

sen_value = pp.Literal("[[").suppress() + HOTLOAD_SENTENCE("internal_sentence") + pp.Literal("]]").suppress()

sen_value.setParseAction(lambda s, l, t: (t["internal_sentence"].type, t["internal_sentence"]))
