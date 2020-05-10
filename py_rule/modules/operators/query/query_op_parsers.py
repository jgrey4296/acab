import pyparsing as pp

from py_rule import util
from py_rule.abstract.parsing import util as PU
from py_rule.abstract.query import QueryComponent

from . import query_operators as QO

def construct_tag_query(toks):
    assert(util.TAG_S in toks)
    tags = [x[1] for x in toks[util.TAG_S]]

    return QueryComponent(QO.HasTag().op_str, param=tags)


tagList = PU.N(util.TAG_S, pp.delimitedList(PU.tagName, delim=","))

tagList.setParseAction(construct_tag_query)
