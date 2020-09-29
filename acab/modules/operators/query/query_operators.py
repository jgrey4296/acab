"""
Definitions of initial Comparison operators
"""
import re

from acab.abstract.rule.query import QueryOp, QueryOp_SubBind


class EQ(QueryOp):
    def __call__(self, a, b, data=None, engine=None):
        return a == b


class NEQ(QueryOp):
    def __call__(self, a, b, data=None, engine=None):
        return a != b


class RegMatch(QueryOp_SubBind):
    def __call__(self, a, b, data=None, engine=None):
        result = re.search(b, a)
        if result is not None:
            result = result.groupdict()
        return result


class ELEM(QueryOp):
    def __call__(self, a, b, data=None, engine=None):
        return a in b


class HasTag(QueryOp):
    def __call__(self, value, *tags, data=None, engine=None):
        return value.has_tag(*tags)
