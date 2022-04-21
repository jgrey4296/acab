import pyparsing as pp
import re

from acab.core.parsing import parsers as PU
from acab.core.config.config import AcabConfig

from acab.core.value.value import AcabValue
from acab.core.value.sentence import Sentence
from acab.core.parsing.annotation import ValueRepeatAnnotation
from acab.core.value.instruction import ProductionComponent
from acab.core.engine.util import prep_op_path
from acab.interfaces.value import ValueFactory_i as VF

from . import query_operators as QO

config       = AcabConfig()
TAG_S        = config.prepare("Value.Structure", "TAG")()
CONSTRAINT_S = config.prepare("Value.Structure", "CONSTRAINT")()

tag_op_path = VF.sen(prep_op_path(__package__, QO.HasTag.__name__))

def construct_tag_query(toks):
    value_tags = toks[0].words

    return ValueRepeatAnnotation(CONSTRAINT_S,
                                 ProductionComponent(value=tag_op_path, params=value_tags))


tagList = PU.tagSen.copy()

tagList.add_parse_action(construct_tag_query)

tagList.set_name("QueryTagList")
