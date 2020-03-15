from pyrule.abstract.agenda import Agenda

# TODO
class ReduceAgenda(Agenda):
    """ Agenda to act like the functional programming concept. Take options and fold them """
    # ie [proposals] + [list_of_chars] -> [total_change_by_char]
    # could include saturation curves/limits

    def __init__(self):
        super().__init__()

    def __call__(self, proposals, engine, **kwargs):
        return
