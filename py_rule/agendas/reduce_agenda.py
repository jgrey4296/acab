from pyrule.abstract.agenda import Agenda

class ReduceAgenda(Agenda):
    """ Agenda to act like the functional programming
    concept. Take options and fold them
    """

    def __init__(self):
        super().__init__()

    def __call__(self):
        return
