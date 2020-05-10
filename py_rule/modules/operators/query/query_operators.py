"""
Definitions of initial Comparison operators
"""
import re

from py_rule.abstract.query import QueryOp


class EQ(QueryOp):
    def __call__(self, a, b, data=None, engine=None):
        return a == b


class NEQ(QueryOp):
    def __call__(self, a, b, data=None, engine=None):
        return a != b


class RegMatch(QueryOp):
    def __call__(self, a, b, data=None, engine=None):
        return re.search(b, a)


class ELEM(QueryOp):
    def __call__(self, a, b, data=None, engine=None):
        return a in b


class HasTag(QueryOp):
    def __call__(self, a, b, data=None, engine=None):
        return all([x in a._tags for x in b])
