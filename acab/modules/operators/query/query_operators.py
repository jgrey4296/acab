"""
Definitions of initial Comparison operators
"""
import re

from acab.abstract.core.production_abstractions import ProductionOperator


class EQ(ProductionOperator):
    def __call__(self, a, b, data=None, engine=None):
        return a == b


class NEQ(ProductionOperator):
    def __call__(self, a, b, data=None, engine=None):
        return a != b


class RegMatch(ProductionOperator):
    # TODO re-implement sub-binds
    def __call__(self, a, b, data=None, engine=None):
        result = re.search(b, a)
        if result is not None:
            result = result.groupdict()
        return result


class ELEM(ProductionOperator):
    def __call__(self, a, b, data=None, engine=None):
        return a in b


class HasTag(ProductionOperator):
    def __call__(self, value, *tags, data=None, engine=None):
        return value.has_tag(*tags)
