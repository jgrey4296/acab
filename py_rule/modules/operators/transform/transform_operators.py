"""
Defintions for Core Transform Operators
"""
from re import sub

from py_rule.abstract.transform import TransformOp

class RegexOp(TransformOp):
    def __init__(self):
        super().__init__(3)

    def __call__(self, a, b, replacement, data=None, engine=None):
        """ Substitute a pattern with a value from passed in data
        a : the replacement
        b: the pattern

        sub(pattern, replacement, string, count, flags)
        """
        return sub(b, replacement, a)


class FormatOp(TransformOp):
    def __init__(self):
        super().__init__(1)

    def __call__(self, a, data=None, engine=None):
        """ Use str.format variant with a data dictionary
        Replaces variables in the string with bound values
        """
        return a.format(**data)
