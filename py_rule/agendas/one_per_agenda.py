from pyrule.abstract.agenda import Agenda

class OnePerAgenda(Agenda):
    """ Agenda to select an option for each
    provided index (eg: character)
    ""

    def __init__(self):
        super().__init__()

    def __call__(self):
        return
