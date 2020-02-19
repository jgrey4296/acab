from pyrule.abstract.agenda import Agenda

class SelectorAgenda(Agenda):
    """ Agenda to pick one of a number of options """"

    def __init__(self):
        super().__init__()

    def __call__(self):
        return
