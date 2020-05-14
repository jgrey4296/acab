from py_rule.abstract.value import PyRuleValue, PyRuleStatement
from py_rule.abstract.printing import util as PrU

class Type(PyRuleValue):
    """ The unrestricted type """

    def __init__(self, value="|∀σ|", **kwargs):
        super().__init__(value, **kwargs)


    @property
    def path(self):
        return self.value
    @property
    def head(self):
        return self.path[-1]
    @property
    def vars(self):
        return self._vars


class TypeStatement(PyRuleStatement):

    def __init__(self, value="|∀σ|", **kwargs):
        super().__init__(value, **kwargs)
        self._path = None


    @property
    def head(self):
        return self.path[-1]
    @property
    def vars(self):
        return self._vars

    @property
    def pprint_has_content(self):
        head_content = any([bool(x) for x in [self._vars,
                                              self._tags]])
        struc_content = self.structure is not None

        return (head_content, struc_content)


    def pprint_body(self, val):
        def_op = PrU.default_opts()
        return val + "\n".join([x.pprint(def_op) for x in self.structure])
