from acab.abstract.core.core_abstractions import AcabStatement

from acab.abstract.config.config import AcabConfig

util = AcabConfig.Get()

TAB_S = util.value("Print.Patterns", "TAB", actions=[AcabConfig.actions_e.STRIPQUOTE])

class TypeStatement(AcabStatement):

    def __init__(self, value="|∀σ|", **kwargs):
        super().__init__(value, **kwargs)
        self._path = None
        self._structure = []


    @property
    def head(self):
        return self.path[-1]
    @property
    def vars(self):
        return self._params

    @property
    def structure(self):
        return self._structure

    @property
    def pprint_has_content(self):
        head_content = any([bool(x) for x in [self._params,
                                              self._tags]])
        struc_content = self.structure is not None

        return (head_content, struc_content)


    def pprint_body(self, val):
        body = ("\n" + TAB_S).join([x.pprint() for x in self.structure])
        return val + TAB_S + body + "\n"
