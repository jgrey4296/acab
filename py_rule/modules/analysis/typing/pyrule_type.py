from py_rule.abstract.value import PyRuleValue


class Type(PyRuleValue):
    """ The unrestricted type """

    def __init__(self, type_str=None):
        super().__init__(type_str=type_str)
        self._name = "|∀σ|"
