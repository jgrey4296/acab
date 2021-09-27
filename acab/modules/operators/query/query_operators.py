"""
Definitions of initial Comparison operators
"""
import re

from acab import types as AT
from acab.abstract.core.production_abstractions import ProductionOperator
from acab.abstract.decorators.semantic import OperatorArgUnWrap, OperatorResultWrap
from acab.abstract.decorators.semantic import OperatorSugar

Value    = AT.Value
Sentence = AT.Sentence

@OperatorSugar("==")
class EQ(ProductionOperator):

    @OperatorArgUnWrap
    def __call__(self, a, b, data=None):
        return a == b


@OperatorSugar("!=")
class NEQ(ProductionOperator):

    @OperatorArgUnWrap
    def __call__(self, a, b, data=None):
        return a != b

@OperatorSugar("~=")
class RegMatch(ProductionOperator):

    # TODO implement sub-binds
    # currently they are ignored
    @OperatorArgUnWrap
    def __call__(self, a, b, data=None):
        result = re.search(b, a)
        if result is not None:
            result = result.groupdict()
        if result is not None and not bool(result):
            result = True
        return result

@OperatorSugar("∈")
class ELEM(ProductionOperator):

    @OperatorArgUnWrap
    def __call__(self, a:Value, b:Sentence, data=None):
        return a in b

class HasTag(ProductionOperator):

    # Doesn't need sugar, as it uses a parser
    # Don't unwrap args, as you need the value data to test
    def __call__(self, value, *tags, data=None):
        return value.has_tag(*tags)
