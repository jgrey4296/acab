"""
Definitions of initial Comparison operators
"""
import re

import acab
from acab import types as AT
from acab.interfaces import value as VI
from acab.core.value.instruction import ProductionOperator
from acab.core.util.decorators.semantic import OperatorArgUnWrap, OperatorResultWrap
from acab.core.util.decorators.semantic import OperatorSugar

config = acab.config

Value    = AT.Value
Sentence = AT.Sentence
CtxIns   = AT.CtxIns

@OperatorSugar(config.any_of().operator.Sugar.EQ())
class EQ(ProductionOperator):

    def __call__(self, a, b, *, data=None, ctx=None):
        if isinstance(b, VI.Sentence_i) and len(b) == 1:
            return a == b[0]
        else:
            return a == b


@OperatorSugar(config.any_of().operator.sugar.NEQ())
class NEQ(ProductionOperator):

    @OperatorArgUnWrap
    def __call__(self, a, b, *, data=None, ctx=None):
        return a != b

@OperatorSugar(config.any_of().operator.sugar.REGEX_MATCH())
class RegMatch(ProductionOperator):

    # TODO implement sub-binds
    # currently they are ignored
    @OperatorArgUnWrap
    def __call__(self, a, b, *, data=None, ctx=None):
        result = re.search(b, a)
        if result is not None:
            result = result.groupdict()
        if result is not None and not bool(result):
            result = True
        return result

@OperatorSugar(config.any_of().operator.sugar.ELEM())
class ELEM(ProductionOperator):

    def __call__(self, a:Value, b:Sentence, *, data=None, ctx=None):
        return a in b

class HasTag(ProductionOperator):

    # Doesn't need sugar, as it uses a parser
    # Don't unwrap args, as you need the value data to test
    def __call__(self, value, *tags, data=None, ctx=None):
        return value.has_tag(*tags)


@OperatorSugar(config.any_of().operator.sugar.TYPEMATCH())
class SimpleTypeMatch(ProductionOperator):
    """ Match a value's type to a passed in sentence """

    def __call__(self, a:Value, b:Sentence, *, data=None, ctx:CtxIns=None):
        a_type = a.type

        # TODO this will eventually be some sort of unify
        if a_type == b:
            return True

        for ax, bx in zip(a_type.words, b.words):
            if ax.is_var:
                ax = ctx[ax]
            if bx.is_var:
                bx = ctx[bx]

            if ax == bx:
                continue
            if ax.is_var or bx.is_var:
                continue

            return False

        return True


class AlwaysMatch(ProductionOperator):
    """ A Simple Operator that always retursn true,
    useful testing"""

    def __call__(self, a, b, *, data=None, ctx=None):
        return True
