from pyrule.abstract.agenda import Agenda

# TODO
class RandomSelectorAgenda(Agenda):
    """ Agenda To select a subset of options, randomly """
    # by an amount, and with a supplied distribution

    def __init__(self):
        super().__init__()

    def __call__(self, proposals, engine, **kwargs):
        return
