from pyrule.abstract.agenda import Agenda

# TODO
class SandboxAgenda(Agenda):
    """ Agenda to Run a selection of options
    in their own sandboxes before selecting a preference """

    def __init__(self):
        super().__init__()

    def __call__(self, proposals, engine, **kwargs):
        return
