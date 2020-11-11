import pyparsing as pp

from acab.abstract.parsing import parsers as PU
from acab.abstract.config.config import AcabConfig

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence

from acab.abstract.rule.query import QueryComponent

from . import query_operators as QO

util         = AcabConfig.Get()
TAG_S        = util.value("Value.Structure", "TAG")
CONSTRAINT_S = util.value("Value.Structure", "CONSTRAINT")


def construct_tag_query(toks):
    assert(TAG_S in toks)
    tags = [x[1] for x in toks[TAG_S]]

    tag_op_path = Sentence.build([QO.HasTag.__name__])
    value_tags = [AcabValue(x) for x in tags]
    return (CONSTRAINT_S, QueryComponent(tag_op_path, param=value_tags))


tagList = PU.N(TAG_S, pp.delimitedList(PU.tagName, delim=","))

tagList.setParseAction(construct_tag_query)
