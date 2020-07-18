"""
Defintions for Core Transform Operators
"""
from re import sub

from acab.abstract.transform import TransformOp

class RegexOp(TransformOp):
    def __init__(self):
        super().__init__(3)

    def __call__(self, value, pattern, replacement, data=None, engine=None):
        """ Substitute value pattern with value value from passed in data
        value : the replacement
        pattern: the pattern

        sub(pattern, replacement, string, count, flags)
        """
        # TODO: use re.RegexFlag 's
        return sub(str(pattern), str(replacement), str(value))


class FormatOp(TransformOp):
    def __init__(self):
        super().__init__(1)

    def __call__(self, value, data=None, engine=None):
        """ Use str.format variant with value data dictionary
        Replaces variables in the string with bound values
        """
        return value.format(**data)
