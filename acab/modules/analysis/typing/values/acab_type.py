from acab.abstract.core.values import AcabStatement

from acab.abstract.config.config import AcabConfig

config = AcabConfig.Get()

TAB_S = config.prepare("Print.Patterns", "TAB", actions=[AcabConfig.actions_e.STRIPQUOTE])()

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
        return self.params

    @property
    def structure(self):
        return self._structure
