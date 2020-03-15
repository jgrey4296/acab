from pyrule.abstract.agenda import Agenda

# TODO
class RankingAgenda(Agenda):
    """ Agenda to Rank options by an index, and select from the highest scoring """
    # ie: [proposals] + ranking.loc.query

    def __init__(self):
        super().__init__()

    def __call__(self, proposals, engine, **kwargs):
        return
