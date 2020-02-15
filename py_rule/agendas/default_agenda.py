from pyrule.abstract.agenda import Agenda

class DefaultAgenda(Agenda):
    """ The Default Agenda for a Layer: Enacts everything """

    def __init__(self):
        super().__init__()

    def __call__(self):
        return
