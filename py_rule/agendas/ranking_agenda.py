from pyrule.abstract.agenda import Agenda

# TODO add control over min/max order, selection range, selection amnt
class RankingAgenda(Agenda):
    """ Agenda to Rank options by an index, and select from the highest scoring """
    # ie: [proposals] + ranking.loc.query

    def __init__(self, ranking_key):
        super().__init__()
        self._ranking_key = ranking_key


    def __call__(self, proposals, engine, **kwargs):
        proposals.sort(key=lambda x: x[0][self._ranking_key])
        return [proposals[-1]]
