import pyparsing as pp

from acab import util
from acab.abstract.parsing import util as PU
from acab.abstract.query import QueryComponent
from acab.abstract.value import AcabValue

from . import query_operators as QO

def construct_tag_query(toks):
    assert(util.TAG_S in toks)
    tags = [x[1] for x in toks[util.TAG_S]]

    return (util.CONSTRAINT_S, QueryComponent(QO.HasTag().op_str, param=tags))


tagList = PU.N(util.TAG_S, pp.delimitedList(PU.tagName, delim=","))

tagList.setParseAction(construct_tag_query)
