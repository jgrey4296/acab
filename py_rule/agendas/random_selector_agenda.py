from pyrule.abstract.agenda import Agenda
from random import shuffle


# TODO add proportions
class RandomSelectorAgenda(Agenda):
    """ Agenda To select a subset of options, randomly """
    # by an amount, and with a supplied distribution

    def __init__(self, amnt):
        super().__init__()
        self._amnt = amnt

    def __call__(self, proposals, engine, **kwargs):
        shuffle(proposals)
        return proposals[:amnt]
