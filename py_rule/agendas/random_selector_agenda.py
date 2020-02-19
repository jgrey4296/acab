from pyrule.abstract.agenda import Agenda

class RandomSelectorAgenda(Agenda):
    """ Agenda To select a subset of options,
    randomly
    """

    def __init__(self):
        super().__init__()

    def __call__(self):
        return
