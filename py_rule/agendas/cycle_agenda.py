from pyrule.abstract.agenda import Agenda

class CycleAgenda(Agenda):
    """ Agenda for Cycling around a list of options """

    def __init__(self):
        super().__init__()

    def __call__(self):
        return
