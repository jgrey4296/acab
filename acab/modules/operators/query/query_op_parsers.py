import pyparsing as pp

from acab import util
from acab.abstract.parsing import util as PU
from acab.abstract.query import QueryComponent
from acab.abstract.value import AcabValue
from acab.abstract.sentence import Sentence

from . import query_operators as QO

def construct_tag_query(toks):
    assert(util.TAG_S in toks)
    tags = [x[1] for x in toks[util.TAG_S]]

    tag_op_path = Sentence.build([QO.HasTag.__name__])
    return (util.CONSTRAINT_S, QueryComponent(tag_op_path, param=tags))


tagList = PU.N(util.TAG_S, pp.delimitedList(PU.tagName, delim=","))

tagList.setParseAction(construct_tag_query)
