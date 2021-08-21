"""
Defintions for Core Transform Operators
"""
from re import sub
from acab.abstract.core.production_abstractions import ProductionOperator
from acab.modules.semantics.util import SemanticOperatorWrapDecorator, SemanticUnWrapData

class RegexOp(ProductionOperator):

    @SemanticOperatorWrapDecorator
    def __call__(self, value, pattern, replacement, data=None):
        """ Substitute value pattern with value value from passed in data
        value : the replacement
        pattern: the pattern

        sub(pattern, replacement, string, count, flags)
        """
        return sub(pattern, replacement, value)


class FormatOp(ProductionOperator):

    @SemanticUnWrapData
    @SemanticOperatorWrapDecorator
    def __call__(self, value:str, data=None):
        """ Use str.format variant with value data dictionary
        Replaces variables in the string with bound values
        """
        return value.format_map(data)
