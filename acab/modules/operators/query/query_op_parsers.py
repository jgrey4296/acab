##-- imports
from __future__ import annotations
import re

import acab.core.defaults.value_keys as DS
import pyparsing as pp
import acab
from acab.core.engine.util import prep_op_path
from acab.core.parsing import parsers as PU
from acab.core.util.annotation import ValueRepeatAnnotation
from acab.core.util.sentences import ProductionComponent
from acab.core.value.sentence import Sentence
from acab.interfaces.value import ValueFactory as VF

from . import query_operators as QO

##-- end imports

config       = acab.config
TAG_S        = DS.TAG
CONSTRAINT_S = DS.CONSTRAINT

tag_op_path = VF.sen(prep_op_path(__package__, QO.HasTag.__name__))

def construct_tag_query(toks):
    value_tags = toks[0].words

    return ValueRepeatAnnotation(CONSTRAINT_S,
                                 ProductionComponent(tag_op_path, params=value_tags))


tagList = PU.tagSen.copy()

tagList.add_parse_action(construct_tag_query)

tagList.set_name("QueryTagList")
