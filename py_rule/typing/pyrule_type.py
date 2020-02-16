from py_rule.abstract.value import PyRuleValue

class Type(PyRuleValue):
    """ An unrestricted type """

    def __repr__(self):
        return "|∀|"
