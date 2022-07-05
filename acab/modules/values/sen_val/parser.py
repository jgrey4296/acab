#!/usr/bin/env python3

import logging as logmod
import pyparsing as pp

from acab.core.parsing.consts import DOUBLEBAR, COLON, COMMA, DBLCOLON, DELIM, component_gap
from acab.core.parsing.consts import N, NG, op, OPAR, CPAR, NEGATION
from acab.core.value.sentence import Sentence
from acab.core.parsing import parsers as PU
from acab.core.defaults import parse_symbols as PDSym
from acab.core.defaults import parse_keys as DK
import acab.core.defaults.value_keys as CDS
from acab.core.util.annotation import ValueAnnotation

from acab.modules.analysis.typing import util as TYU

from acab.core.config.config import AcabConfig

def build_flatten(s, l, t):
    value = True
    if 'sharp' in t:
        value = False

    if DK.NEGATION in t:
        value = not value

    annot = ValueAnnotation(CDS.FLATTEN, value)
    return annot

HOTLOAD_SENTENCE = pp.Forward()
HOTLOAD_SENTENCE.set_name("hotload_sentence")

sen_value = pp.Literal("[[").suppress() + HOTLOAD_SENTENCE("internal_sentence") + pp.Literal("]]").suppress()
sen_value.set_name("sen_val")
sen_value.set_parse_action(lambda s, l, t: (t["internal_sentence"].type, t["internal_sentence"]))

# TODO : maybe use double sharp/flat
flatten_annotation  = op(NEGATION) + (pp.Literal(PDSym.FLATTEN)('flat') | pp.Literal(PDSym.SHARP)('sharp'))
flatten_annotation.set_name("FlattenAnno")
flatten_annotation.set_parse_action(build_flatten)
