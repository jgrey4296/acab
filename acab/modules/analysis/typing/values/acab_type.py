from acab.abstract.value import AcabStatement
from acab.abstract.printing import util as PrU
from acab.util import TAB_S

class TypeStatement(AcabStatement):

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
        return val + TAB_S + "\n{}".format(TAB_S).join([x.pprint() for x in self.structure])
