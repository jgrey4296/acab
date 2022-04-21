"""
Defintions for Core Transform Operators
"""
from re import sub

from acab.core.value.instruction import ProductionOperator
from acab.core.util.decorators.semantic import (OperatorArgUnWrap,
                                               OperatorDataUnWrap,
                                               OperatorResultWrap,
                                               OperatorSugar)

@OperatorSugar("~:")
class RegexOp(ProductionOperator):

    @OperatorArgUnWrap
    @OperatorResultWrap
    def __call__(self, value, pattern, replacement, data=None):
        """ Substitute value pattern with value value from passed in data
        value : the replacement
        pattern: the pattern

        sub(pattern, replacement, string, count, flags)
        """
        return sub(pattern, replacement, value)

@OperatorSugar("%:")
class FormatOp(ProductionOperator):

    @OperatorDataUnWrap
    @OperatorArgUnWrap
    @OperatorResultWrap
    def __call__(self, value:str, data=None):
        """ Use str.format variant with value data dictionary
        Replaces variables in the string with bound values
        """
        return value.format_map(data)
