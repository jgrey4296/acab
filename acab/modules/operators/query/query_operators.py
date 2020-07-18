"""
Definitions of initial Comparison operators
"""
import re

from acab.abstract.query import QueryOp


class EQ(QueryOp):
    def __call__(self, a, b, data=None, engine=None):
        return a == b


class NEQ(QueryOp):
    def __call__(self, a, b, data=None, engine=None):
        return a != b


class RegMatch(QueryOp):
    def __call__(self, a, b, data=None, engine=None):
        # TODO: use re.RegexFlag 's 
        return re.search(b, a)


class ELEM(QueryOp):
    def __call__(self, a, b, data=None, engine=None):
        return a in b


class HasTag(QueryOp):
    def __call__(self, value, *tags, data=None, engine=None):
        return value.has_tag(*tags)
