from pyrule.abstract.agenda import Agenda

# TODO
class ExpanderAgenda(Agenda):
    """ Agenda to expand a grammar of options up to needed amount """
    # eg: action "tick hunger" is expanded for all characters

    def __init__(self, expand_by=None):
        super().__init__()
        self._expand_by = expand_by

    def __call__(self, proposals, engine, **kwargs):
        expanded = []
        # for all proposals

        # use expand_by to duplicate actions

        return expanded
