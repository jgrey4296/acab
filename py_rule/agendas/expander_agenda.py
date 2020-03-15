from pyrule.abstract.agenda import Agenda

# TODO
class ExpanderAgenda(Agenda):
    """ Agenda to expand a grammar of options up to needed amount """
    # eg: action "tick hunger" is expanded for all characters

    def __init__(self):
        super().__init__()

    def __call__(self, proposals, engine, **kwargs):
        return
