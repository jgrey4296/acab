from pyrule.abstract.agenda import Agenda
from random import shuffle


# TODO add proportions
class RandomSelectorAgenda(Agenda):
    """ Agenda To select a subset of options, randomly """
    # by an amount, and with a supplied distribution

    def __init__(self, amnt=20, dist=None):
        super().__init__()
        self._amnt = amnt
        self._curve = curve

    def __call__(self, proposals, engine, **kwargs):
        # TODO: replace shuffle with a call to the engine random generator,
        # using the chosen distribution
        shuffle(proposals)
        return proposals[:amnt]
