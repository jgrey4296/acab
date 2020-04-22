from py_rule.abstract.value import PyRuleValue, PyRuleStatement


class Type(PyRuleValue):
    """ The unrestricted type """

    def __init__(self, value="|∀σ|", type_str=None, **kwargs):
        super().__init__(value, type_str=type_str, **kwargs)

    @property
    def path(self):
        return self._path
    @property
    def head(self):
        return self.path[-1]._value
    @property
    def vars(self):
        return self._vars


class TypeStatement(PyRuleStatement):

    def __init__(self, value="|∀σ|", type_str=None, **kwargs):
        super().__init__(value, type_str=type_str, **kwargs)

    @property
    def path(self):
        return self._path
    @property
    def head(self):
        return self.path[-1]._value
    @property
    def vars(self):
        return self._vars



