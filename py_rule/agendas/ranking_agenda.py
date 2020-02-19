from pyrule.abstract.agenda import Agenda

class RankingAgenda(Agenda):
    """ Agenda to Rank options by an index,
    and select from the highest scoring
    """

    def __init__(self):
        super().__init__()

    def __call__(self):
        return
