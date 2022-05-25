"""
Defintions for Core Transform Operators
"""
from re import sub
from acab import AcabConfig

from acab.core.value.instruction import ProductionOperator
from acab.core.util.decorators.semantic import (OperatorArgUnWrap,
                                               OperatorDataUnWrap,
                                               OperatorResultWrap,
                                               OperatorSugar)

config = AcabConfig()

@OperatorSugar(config.attr.Operator.Sugar.REGEX_TRANSFORM)
class RegexOp(ProductionOperator):

    @OperatorArgUnWrap
    @OperatorResultWrap
    def __call__(self, value, pattern, replacement, *, data=None, ctx=None):
        """ Substitute value pattern with value value from passed in data
        value : the replacement
        pattern: the pattern

        sub(pattern, replacement, string, count, flags)
        """
        return sub(pattern, replacement, value)

@OperatorSugar(config.attr.Operator.Sugar.FORMAT)
class FormatOp(ProductionOperator):

    @OperatorDataUnWrap
    @OperatorArgUnWrap
    @OperatorResultWrap
    def __call__(self, value:str, *, data=None, ctx=None):
        """ Use str.format variant with value data dictionary
        Replaces variables in the string with bound values
        """
        return value.format_map(data)
