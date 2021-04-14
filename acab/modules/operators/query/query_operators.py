"""
Definitions of initial Comparison operators
"""
import re

from acab.abstract.core.production_abstractions import ProductionOperator
from acab.modules.semantics.util import SemanticTestWrapDecorator

class EQ(ProductionOperator):
    @SemanticTestWrapDecorator
    def __call__(self, a, b, data=None):
        return a == b


class NEQ(ProductionOperator):
    @SemanticTestWrapDecorator
    def __call__(self, a, b, data=None):
        return a != b


class RegMatch(ProductionOperator):
    # TODO re-implement sub-binds
    @SemanticTestWrapDecorator
    def __call__(self, a, b, data=None):
        result = re.search(b, a)
        if result is not None:
            result = result.groupdict()
        return result


class ELEM(ProductionOperator):
    @SemanticTestWrapDecorator
    def __call__(self, a, b, data=None):
        return a in b


class HasTag(ProductionOperator):
    @SemanticTestWrapDecorator
    def __call__(self, value, *tags, data=None):
        return value.has_tag(*tags)
