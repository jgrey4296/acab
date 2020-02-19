from pyrule.abstract.agenda import Agenda

class ExpanderAgenda(Agenda):
    """ Agenda to expand a grammar of options
    up to needed amount
    """

    def __init__(self):
        super().__init__()

    def __call__(self):
        return
