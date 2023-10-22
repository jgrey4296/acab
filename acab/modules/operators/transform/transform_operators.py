"""
Defintions for Core Transform Operators
"""
##-- imports
from __future__ import annotations
from re import sub

import acab
from acab.interfaces import value as VI
from acab.interfaces.value import ValueFactory as VF
from acab.core.value.instruction import ProductionOperator
from acab.core.util.decorators.semantic import (OperatorArgUnWrap,
                                               OperatorDataUnWrap,
                                               OperatorResultWrap,
                                               OperatorSugar)

##-- end imports

config = acab.config

@OperatorSugar(config.any_of().operator.sugar.REGEX_TRANSFORM())
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
                if len(words) == 1:
                    return VF.value(data=value[0].data, value=words[0])
                return value.copy(value=words)
            case VI.Value_i():
                subbed = sub(pattern, replacement, value.value)
                return value.copy(value=subbed, name=subbed)
            case _:
                raise TypeError("Unrecognized type to RegexOp", value)

@OperatorSugar(config.any_of().operator.sugar.FORMAT())
class FormatOp(ProductionOperator):

    @OperatorDataUnWrap
    @OperatorArgUnWrap
    @OperatorResultWrap
    def __call__(self, value:str, *, data=None, ctx=None):
        """ Use str.format variant with value data dictionary
        Replaces variables in the string with bound values
        """
        return value.format_map(data)
