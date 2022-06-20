"""
Defintions for Core Transform Operators
"""
from re import sub
from acab import AcabConfig

from acab.interfaces import value as VI
from acab.interfaces.value import ValueFactory as VF
from acab.core.value.instruction import ProductionOperator
from acab.core.util.decorators.semantic import (OperatorArgUnWrap,
                                               OperatorDataUnWrap,
                                               OperatorResultWrap,
                                               OperatorSugar)

config = AcabConfig()

@OperatorSugar(config.attr.Operator.Sugar.REGEX_TRANSFORM)
class RegexOp(ProductionOperator):

    def __call__(self, value, pattern, replacement, *, data=None, ctx=None):
        """ Substitute value pattern with value value from passed in data
        value : the replacement
        pattern: the pattern

        sub(pattern, replacement, string, count, flags)
        """
        pattern     = pattern[0].value
        replacement = replacement[0].value
        match value:
            case VI.Sentence_i():
                words = [sub(pattern, replacement, x.value) for x in value.words]
                return value.copy(words)
            case VI.Value_i():
                subbed = sub(pattern, replacement, value.value)
                return value.copy(value=subbed, name=subbed)
            case _:
                raise TypeError("Unrecognized type to RegexOp", value)

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
